--
-- The 'meat' of the interpreter.
-- It describes how the AST of the program should be interpreted.
--

{-# LANGUAGE FlexibleInstances #-}

module Interpreter (
  translate
  ) where

import Control.Applicative (Alternative(..))
import Control.Lens (over, (^.))
import Control.Monad (when, forM, forM_, foldM, unless, msum)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.Loops (iterateUntilM, whileM_)
import Control.Monad.Reader (local)
import Control.Monad.Trans.State (get, modify)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isNothing)
import Debug.Trace (trace)

import Context
import ErrM (Err(..))
import Flags
import Grammar
import Operators
import PrintOcaml
import StaticBinding
import TypeConstraint (fromIntToTerm, OpenTerm)
import Types
import UtilsOcaml

-- Pattern matching

instance Semigroup (Err [LabeledStatement]) where
  (<>) (Bad s) _ = Bad s
  (<>) _ (Bad s) = Bad s
  (<>) (Ok m1) (Ok m2) = Ok (m1 <> m2)

zipMatchings :: [Expression] -> [Expression] -> Err [LabeledStatement]
zipMatchings vexprs pexprs = case compare (length vexprs) (length pexprs) of
  EQ -> foldl (<>) (Ok []) $ zipWith pushMatching vexprs pexprs
  LT -> Bad "(1) Partial application is not yet supported."
  _ -> Bad "(2) Wrong tuple size."

pushMatching :: Expression -> Expression -> Err [LabeledStatement]
pushMatching ve pe = case (pe, ve) of 
  (ETuple pexprs, ETuple vexprs) -> zipMatchings vexprs pexprs
  (ENamedTuple pname pexprs, ENamedTuple vname vexprs) | pname == vname && not (null pexprs) -> 
    zipMatchings vexprs pexprs
  (EVar (VConstructor pname), EVar (VConstructor vname)) | pname == vname -> Ok []
  (EVar (VSimple VBlank), _) -> Ok []
  (EVar pv@(VSimple _),_) -> Ok [SLabeled pv ve]
  (EConst _, EConst _) | pe == ve -> Ok []
  (EConst _, EConst _) -> Bad "(3) Constants do not match."
  _ -> Bad $ "(4) Invalid pattern matching.\n" ++ printTree pe ++ "\n" ++ printTree ve

-- Function applications

applyFunctionCall :: Expression -> [Expression] -> AResult Expression
applyFunctionCall f expressions = case f of
  EVar (VOpTuple T) -> eval $ ETuple expressions
  EVar (VConstructor name) -> eval $ ENamedTuple name expressions
  ELambda {} -> applyLambda f expressions
  _ -> bad $ "Expression not callable.. " ++ printTree f

applyFoldrFunctionCall :: Expression -> [Expression] -> Expression -> AResult Expression
applyFoldrFunctionCall f expressions acc = case f of
  ELambda {} -> foldM applyFunc acc (reverse expressions) where
    applyFunc acc lhs = applyLambda f [lhs, acc]
  EVar (VConstructor name) -> foldM applyFunc acc (reverse expressions) where
    applyFunc acc lhs = return $ ENamedTuple name [lhs, acc] 
  _ -> bad $ "Expected a lambda." ++ printTree f

-- TODO Make it less hacky....
-- acc (v, e) -> let temp = e in match temp with v -> acc
applyPartialFunction :: Expression -> (Expression, Expression) -> AResult Expression
applyPartialFunction acc (v, e) = do
  temp <- local (setLocal . setNext) $ fvBind temporaryVariable
  let match = EMatch [temp] [Matching1 [v] acc]
  return $ ELocalDefinition (SValue RecNo [VSimple temp] e) match

-- TODO fix zero parameter calls?
applyLambda :: Expression -> [Expression] -> AResult Expression
applyLambda x expressions = case x of
  ELambda ref matching@(Matching1 patterns expression) -> do
    let len_expr = length expressions
    let len_pat = length patterns
    case compare len_expr len_pat of
      LT -> do
        let (ps1, ps2) = splitAt len_expr patterns
        let ps1' = applyTuple ps1
        let expressions' = applyTuple expressions
        new_expression <- applyPartialFunction expression (ps1', expressions')
        return $ toLambda ps2 new_expression
      EQ -> do
        es <- forM expressions eval
        let e = applyTuple es
        applyMatch e [matching]
      GT -> bad $ "Too many parameters applied to function call. (" 
        ++ show len_expr  ++ ">" ++ show len_pat ++ ")"
  _ -> bad "Expected a lambda."

applyMatch, applyMatchHelper :: Expression -> [Matching] -> AResult Expression
applyMatch expr x =
  applyMatchHelper expr x `catchError`
    appendTrace (ERUnknown $ "match " ++ printTree expr ++ " with " ++ printTree x)

applyMatchHelper e x = case x of
  [] -> throwError $ ERNoMatching e
  Matching1 patterns expression:ms -> do
    let pe = applyTuple patterns
    let result = pushMatching e pe
    case result of
      Bad s -> applyMatchHelper e ms
      Ok lstmts -> withLocals lstmts $ eval expression 

applyRecord :: [LabeledStatement] -> [Expression] -> Expression
applyRecord labeledstatements expressions =
  ERecord $ zipWith replaceLabeledStatement labeledstatements expressions

applyTuple :: [Expression] -> Expression
applyTuple = typeMerge ETuple

-- Evaluators

class Evaluable a where
  eval :: a -> AResult Expression

instance Evaluable LabeledStatement where
  eval x = eval $ x ^. lensExpressionFromLabeledStatement

instance Evaluable CanonicalName where
  eval x = case x of
    CanonicalName {} -> peekFromEnvironment x
    CanonicalModule {} -> undefined {- It should never occur -}

instance Evaluable SimpleVariable where
  eval x = case x of
    VVariable names -> bad $ "Not canonical variable: " ++ show x
    VBlank -> bad $ show x
    VCanonical name -> eval name 

instance Evaluable SequencingExpression where
  eval x = case x of
    ESeq e -> eval e
    EAssign {} -> do
      translate x
      return ENull

instance Evaluable Expression where
  eval x = case x of
    EConst constant -> 
      return x
    EString string -> 
      return x
    EVar variable -> 
      eval variable
    EReferenceMemory ptr -> 
      return x -- [TODO] ?
    EOp1 {} -> undefined -- It should not happen, after fvBind.
    EOp2 {} -> undefined
    ERecord lstmts -> do
      exprs <- forM lstmts eval
      return $ applyRecord lstmts exprs
    EIfThenElse condition expression1 expression2 -> do
      b <- eval condition
      eval $ if isTrue b then expression1 else expression2
    EMatch simplevariables matchings -> do
      es <- forM simplevariables (eval . EVar. VSimple)
      let e = applyTuple es
      applyMatch e matchings
    ETypeOf expression -> 
      applyTypeOf expression
    ELocalDefinition definition expression -> do
      lstmt <- toLabeledStatementFromDefinition definition
      lstmt' <- case lstmt of
        SLabeled v ELambda{} -> return lstmt
        SLabeled v e -> SLabeled v <$> eval e
      withLocal lstmt' $ eval expression
    EStack seq -> last <$> forM seq eval
    EFor var e_start to e_end e_loop -> do
      EConst (EInt i_start) <- eval e_start
      EConst (EInt i_end) <- eval e_end
      let step i = withLocal (SLabeled var (EConst (EInt i))) (eval $ EStack e_loop)
      let (cond, update) = if to == ForTo 
                           then ((> i_end), (+1)) 
                           else ((< i_end), \i -> i - 1)
      iterateUntilM cond (\i -> do step i; return $ update i) i_start
      return ENull
    EWhile condition expressions -> do
      whileM_ (isTrue <$> eval condition) (eval $ EStack expressions)
      return ENull
    EFunctionCall variable expressions -> do {
        pushToOstreamFlagged FTraceFunctionCalls $ "Calling " ++ printTree variable ++ " on " ++ printTree expressions;
        f <- eval variable;
        applyFunctionCall f expressions
      } `catchError` appendTrace (
        ERUnknown $ "Function call failed: " ++ printTree variable ++ "\n with arguments: " ++ printTree expressions
      )
    EExternalFunctionCall binding expressions -> do {
      es' <- forM expressions eval;
      evalExternal binding es'
    } `catchError` appendTrace (
      ERUnknown $ "External function call failed: " ++ show binding ++ "\n with arguments: " ++ printTree expressions
     )
    ETuple expressions -> do
      es <- forM expressions eval
      -- assert $ length expressions > 1 ?
      return $ applyTuple es
    ENamedTuple constructorname expressions -> do
      es <- forM expressions eval
      return $ ENamedTuple constructorname es
    ERecursiveFunctionCall variable expressions expression -> do
      f <- eval variable
      -- TODO  99_EitherList: Allow lazy evaluations when merging lists: List rec (..., l1).
      es <- forM expressions eval
      e <- eval expression
      applyFoldrFunctionCall f es e
    ELambda functionprefix matching -> 
      return x
    ERaise exceptionname expression -> do
      let NException (Ident name) = exceptionname
      te <- eval expression
      throwError $ ERException name (printTree te)
    _ -> error (show x)

instance Evaluable Variable where
  eval x = case x of
    VSimple simplevariable -> eval simplevariable
    VOpTuple tupleconstr -> return . EVar $ x
    VOp op -> undefined -- It should not occur.
    VConstructor names -> return . EVar $ x
 
-- Interpreters

class Translatable a where
  translate :: a -> Result

instance Translatable ModuleContent where
  translate x = do
    lstmt <- local setNoOutput (toLabeledStatementFromModuleContent x) >>= typeBindLabeled
    case lstmt of
      SLabeledBound v i e -> do
        stmt <- SLabeledBound v i <$> eval e
        scope <- askE
        when (scope^.isprinting) (do traceOutputLabeledStatement stmt; return ())
        pushToEnvironment stmt
      _ -> badInternal 4 $ "translate (ModuleContent) error. Labeled statement not bound: " ++ show x

instance Translatable Prog where
  translate (Prog1 statements) = forM_ statements transProgStatement

transProgStatement :: Statement -> Result
transProgStatement s = do 
  traceInput s
  pushToOstreamFlagged FTraceInputTree $ " input: " ++ show s
  fvs <- fvBind s
  pushToOstreamFlagged FTraceInputVersion $ "   ver: " ++ show fvs
  unless (isModuleDocs fvs) $ do
    t <- typeResolveStatement fvs
    case t of 
      Just x -> pushToOstream $ "  type: " ++ printTree x
      Nothing -> pushToOstream "unit"
  -- traceEnvironment
  translate (fvs :: Statement)

instance Translatable Signature where
  translate (Signature1 variable type_) = do -- TODO. this should be in StaticBinding.hs 
    v <- local setNext (fvBind variable) -- TODO debug, it will probably break in local nested statements
    case v of
      VSimple (VCanonical (CanonicalName cindex _ _)) -> do
        let type_' = typeFlatten type_
        typeAssign v type_'
        fvUnbind cindex
      _ -> bad $ "Unable to bind variable... " ++ printTree v

instance Translatable SequencingExpression where
  translate x = case x of
    EAssign (VSimple (VCanonical name)) expression -> do
      e' <- eval expression
      pushAssign name e'
    _ -> bad "Expected canoncial name."

instance Translatable Expression where
  translate x = case x of
    EVar (VSimple (VCanonical m@(CanonicalModule mname))) ->
      traceModuleDocs m
    _ -> do
      traceOutput $ eval x
      return ()

translateModule :: Maybe (Map.Map Ident Type) -> [ModuleContent] -> AResult [(Ident, OpenTerm)]
translateModule interface contents =
    -- [TODO] Modules are also versioned ...
  forM contents $ \mc -> do
    lstmt <- toLabeledStatementFromModuleContent mc
    -- Workaround: setNext ensures that no new IntVar is created.
    lstmt <- local setNext (typeBindLabeled lstmt)
    case lstmt of
      SLabeledBound v@(VSimple (VCanonical name@(CanonicalName (_, ident) _ _))) i e -> do
        -- Unifies signature, when it exists.
        forM_ (interface >>= Map.lookup ident) $ typeUnifySignature i name

        lstmt <- SLabeledBound v i <$> eval e
        pushToEnvironment lstmt
        return (ident, fromIntToTerm i)
      _ -> undefined

instance Translatable Statement where
  translate x = case x of
    SDirective filename -> 
      return () -- Directives are preprocessed
    SEnable flag ->
      flagEnable flag
    SExpression expression -> 
      translate expression
    SExpressionWithType expression type_ -> do
      traceOutput $ eval expression
      return ()
    SSignature signature1 ->
      translate signature1
    STypedef {} ->
      pushTypedef x
    SModuleContent modulecontent ->
      translate modulecontent
    SModuleSignature name signatures1 -> do
      let signatures1' = map (\(Signature1 v t) -> Signature1 v (typeFlatten t)) signatures1
      modify $ over (hmodules . signatures) (Map.insert name signatures1')
    SModuleDefinition smodule modulecontents -> do
      interface <- typeGetInterface smodule
      let modname = smodule ^. lensNameFromModuleName
      terms <- local (setNoOutput . setModuleName modname) $
        translateModule (Map.fromList <$> interface) modulecontents
      typeResolveConstraints
      typeCheckSignatures interface (Map.fromList terms)

-- External functions
evalExternal :: String -> [Expression] -> AResult Expression
evalExternal binding exprs = case (binding, exprs) of
  -- TODO It should check types....
  ("ocaml_neg_int", [e]) -> applyPrefixOp "-" e
  ("ocaml_add_int", [e1, e2]) -> applyInfixOp "+" e1 e2
  ("ocaml_sub_int", [e1, e2]) -> applyInfixOp "-" e1 e2
  ("ocaml_mul_int", [e1, e2]) -> applyInfixOp "*" e1 e2
  ("ocaml_div_int", [e1, e2]) -> applyInfixOp "/" e1 e2
  ("ocaml_mod_int", [e1, e2]) -> applyInfixOp "mod" e1 e2

  ("ocaml_mul_float", [e1, e2]) -> applyInfixOp "*" e1 e2

  ("ocaml_lt", [e1, e2]) -> applyBoolInfixOp "<" e1 e2
  ("ocaml_lte", [e1, e2]) -> applyBoolInfixOp "<=" e1 e2
  ("ocaml_gt", [e1, e2]) -> applyBoolInfixOp ">" e1 e2
  ("ocaml_gte", [e1, e2]) -> applyBoolInfixOp ">=" e1 e2
  ("ocaml_eq", [e1, e2]) -> applyBoolInfixOp "==" e1 e2
  _ -> bad $ "Function binding not available: " ++ binding
