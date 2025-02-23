--
-- The 'meat' of the interpreter.
-- It describes how the AST of the program should be interpreted.
--
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Interpreter
  ( prettyEval
  ) where

import GHC.Float
import GHC.Integer

import Control.Lens ((^.), over)
import Control.Monad (forM_)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Extra (firstJustM)
import Control.Monad.Loops (iterateUntilM, whileM_)
import Control.Monad.Reader (lift, liftIO, local)
import Control.Monad.Trans.State (modify)
import Data.Functor ((<&>))
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as Map

import Context
import Flags
import Grammar
import Operators
import PatternMatching
import Print
import StaticBinding
import Types

applyExternal :: String -> [Expr] -> InterpreterT Expr
applyExternal binding exprs = do
  case exprs of
    [e] ->
      case binding of
        "%raise" -> throwError $ ERException e
        "%identity" -> return e
        "%negint" -> return $ apply1 negateInteger e
        "%boolnot" -> return $ apply1 not e
        "%negfloat" -> return $ apply1 negateDouble e
        "to_string" -> return (from $ printTree e :: Expr)
        "type_of" -> undefined
        "debug" -> return (from True :: Expr)
        _ ->
          bad Nothing
            $ "Prefix function binding not available: " ++ show binding
    [e1, e2] ->
      case binding of
        "%addint" -> return $ apply2 plusInteger e1 e2
        "%subint" -> return $ apply2 minusInteger e1 e2
        "%mulint" -> return $ apply2 timesInteger e1 e2
        "%divint" ->
          if (to e2 :: Integer) == 0
            then throwError $ ERInternal "Divide by zero"
            else return $ apply2 divInteger e1 e2
        "%modint" -> return $ apply2 modInteger e1 e2
        "%addfloat" -> return $ apply2 plusDouble e1 e2
        "%subfloat" -> return $ apply2 minusDouble e1 e2
        "%mulfloat" -> return $ apply2 timesDouble e1 e2
        "%divfloat" -> return $ apply2 divideDouble e1 e2
        "%lessthan" -> return . from $ e1 < e2
        "%lessequal" -> return . from $ e1 <= e2
        "%greaterthan" -> return . from $ e1 > e2
        "%greaterequal" -> return . from $ e1 >= e2
        "%eq" -> return . from $ e1 == e2
        "%notequal" -> return . from $ e1 /= e2
        _ ->
          bad Nothing $ "Infix function binding not available: " ++ show binding
    _ -> bad Nothing $ "Incorrect number of parameters to " ++ show binding

applyLambda :: Expr -> [Expr] -> InterpreterT Expr
applyLambda x expressions =
  case x of
    ELambda _ _ ps e -> applyLambda (EPartial ps [] e) expressions
    EPartial patterns parts e -> do
      es' <- mapM eval expressions
      let parts' = reverse es' ++ parts
      case compare (length patterns) (length parts') of
        GT -> return $ EPartial patterns parts' e
        EQ ->
          applyMatch
            (List npos $ reverse parts')
            [LetPattern npos (List npos patterns) e]
        LT -> bad Nothing "You have passed to many arguments"
    _ -> bad Nothing $ "Expected a lambda, got " ++ show x

applyMatch :: Expr -> [Expr] -> InterpreterT Expr
applyMatch expr patterns = do
  mres <-
    firstJustM
      (\pattern' -> do
         (p, e) <-
           case pattern' of
             LetPattern _ p e -> return (p, e)
             ELambda _ _ [p] e -> return (p, e)
             _ -> bad Nothing $ "Invalid pattern, got " ++ show pattern'
         case match p expr of
           Left err ->
             if length patterns == 1
               then bad Nothing err
               else return Nothing
           Right matches -> Just <$> prepare matches (eval e))
      patterns
  case mres of
    Nothing -> throwError $ ERNoMatching expr patterns
    Just e -> return e

extractSignatures :: Toplevel -> InterpreterT [Statement]
extractSignatures x =
  case x of
    Toplevel1 _ stmts ->
      mapM
        (\case
           Signature pos (ETyped _ v t) -> do
             pos' <- getPosition pos
             t' <- local (setTag TagType) (bind t)
             return $ Signature pos' (ETyped npos v t')
           other ->
             bad (hasPosition other)
               $ "Invalid statement in typemodule. " ++ printTree other)
        stmts
    Toplevel2 {} ->
      bad (hasPosition x) "Expressions are not allowed inside typemodule."

loadSignature :: Path -> InterpreterT ()
loadSignature sig = do
  context <- getContext
  let sigs = context ^. htypes . modulesignatures
  case Map.lookup sig sigs of
    Nothing ->
      bad Nothing
        $ intercalate "\n"
        $ ["Cannot find signature " ++ printTree sig, "Available signatures:"]
            ++ map printTree (Map.keys sigs)
    Just stmts ->
      forM_ stmts $ \case
        Signature _ (ETyped _ v t) -> do
          v' <- bind v
          assertSignature v' t
        _ -> undefined

class Evaluable a where
  eval :: a -> InterpreterT Expr
  prettyEval :: a -> InterpreterT ()
  prettyEval = undefined

instance Evaluable Expr where
  eval x =
    case x of
      EAppend pos e es -> do
        e' <- eval e
        es' <- eval es
        case es' of
          Nil2 _ -> return $ List npos [e']
          List _ elems -> return $ List npos $ e' : elems
          _ -> bad pos $ "Expected a list, got " ++ show es'
    -- This is a bad edge case ... I am sad to hardcode this...
      EApply _ (VCanonical _ (CanonicalName _ [] i 0 [])) [e]
        | i == ILowercase npos (LowercaseIdent "__typeof__") -> do
          t <- typeOf e
          _ <- eval e
          return (from $ printTree t :: Expr)
      EApply _ f es -> do
        f' <- eval f
        applyLambda f' es
      EAssign _ v e -> do
        mref <- getMemory v
        case mref of
          Nothing -> bad Nothing $ "Invalid reference " ++ printTree v
          Just ref' -> do
            e' <- eval e
            saveRef ref' e'
        return unit
      EBegin _ e -> eval e
      EConstant {} -> return x
      EConstructor _ v e -> EConstructor npos v <$> eval e
      EExternalLambda binding _ es -> do
        es' <- mapM eval es
        es'' <-
          mapM
            (\case
               Reference ref -> liftIO $ fromIODeep ref
               other -> return other)
            es'
        applyExternal binding es''
      EFor _ (LetPattern _ (VCanonical _ name) e_start) dir e_end e_loop -> do
        EConstant _ (CInteger _ i_start) <- eval e_start
        EConstant _ (CInteger _ i_end) <- eval e_end
        {- HLINT ignore "Use let" -}
        cond <-
          return
            $ case dir of
                ForTo _ -> (> i_end)
                _ -> (< i_end)
        let update i = do
              context <- getContext
              i' <-
                liftIO
                  $ newIORef
                      (File
                         ((from :: Integer -> Expr) i)
                         (context ^. htypes . tinteger))
              _ <- local (withLocal (name, i')) (eval e_loop)
              return
                $ case dir of
                    ForTo _ -> i + 1
                    _ -> i - 1
        _ <- iterateUntilM cond update i_start
        return unit
      EIf _ condition expression -> do
        b <- eval condition
        if isTrue b
          then eval expression
          else return unit
      EIfElse _ condition expression1 expression2 -> do
        b <- eval condition
        eval
          $ if isTrue b
              then expression1
              else expression2
      ELambda {} -> return x
      ELetIn _ _ defs e -> do
        locals <-
          mapM
            (\case
               LetPattern pos v e2 ->
                 case v of
                   EApply _ (VCanonical _ name) es ->
                     return [(name, toLambda es e2)]
                   _ -> do
                     e2' <- eval e2
                     case match v e2' of
                       Left err -> bad pos err
                       Right matches -> return matches
               _ -> undefined)
            defs
        prepare (concat locals) (eval e)
      EMatch _ e _ patterns -> do
        e' <- eval e
        applyMatch e' patterns
      EPartial {} -> applyLambda x []
      ESeq _ e es -> last <$> mapM eval (e : es)
      ETuple _ e es -> ETuple npos <$> eval e <*> mapM eval es
      ETyped _ e _ -> eval e
      EWhile _ condition expression -> do
        whileM_ (isTrue <$> eval condition) (eval expression)
        return unit
      List _ es -> List npos <$> mapM eval es
      Nil2 _ -> return $ Nil2 npos
      Record _ fields ->
        Record npos
          <$> mapM
                (\case
                   EField _ v e -> EField npos (stripPosition v) <$> eval e
                   other ->
                     bad (hasPosition other)
                       $ "Cannot evaluate record declaration " ++ show other)
                fields
      Reference {} -> return x
      VCanonical {} -> do
        mref <- getMemory x
        mres <-
          case mref of
            Nothing -> bad Nothing "Variable not in memory"
            Just ref -> Context.read [] ref
        case mres of
          Just e -> return e
          _ -> bad Nothing "Read nothing"
        `catchError` appendTrace (ERUnknown Nothing $ "Cannot read " ++ show x)
      _ -> bad (hasPosition x) $ "Cannot evaluate " ++ show x

instance Evaluable Statement where
  eval x =
    case x of
      Definition pos _ defs -> do
        forM_ defs $ \case
          LetPattern _ v e -> do
            e' <- eval e
            case match v e' of
              Left err -> bad pos err
              Right matches ->
                forM_
                  matches
                  (\(name, e'') -> do
                     save name e''
                     let v' = VCanonical npos name
                     t <- typeOf v'
                     Context.log $ LogDefinition v' t e'')
          other -> bad pos $ "Invalid definition " ++ show other
        return unit
      Directive2 pos (DirectiveIdent "#use") (EConstant _ (CString _ path)) -> do
        context <- getContext
        path' <- matchFilePath path
        case path' of
          Nothing -> bad Nothing $ "Could not find file: " ++ show path
          Just path'' -> do
            s <- liftIO $ Prelude.readFile path''
            let s' = (context ^. hfiles . lexer) s
            case (context ^. hfiles . parser) s' of
              Left msg -> throwError $ ERParseFailed path'' msg
              Right tree ->
                local (setCurrentFile path'') (prettyEval tree)
                  `catchError` (\e -> do
                                  pos' <- getPosition pos
                                  appendTrace
                                    (ERUnknown pos'
                                       $ "Use failed: "
                                           ++ show path
                                           ++ " resolving to "
                                           ++ show path'')
                                    e)
        return unit
      EndStmt _ stmt -> eval stmt
      Exception _ (ConstrOf _ v@(VCanonical _ name) _) -> do
        p <- local (setNameContext CtxLocal) (bind temporaryVariable)
        let e = ELambda npos (Fun1 npos) [p] (EConstructor npos v p)
        save name e
        t <- typeOf v
        Context.log $ LogDefinition v t e
        return unit
      Expression _ e -> do
        e' <- eval e
        t <- typeOf e'
        Context.log $ LogOutput t e'
        return unit
      External _ (LetPattern _ (VCanonical _ name) expr) -> do
        save name expr
        return unit
      Module pos (LetPattern _ (Variable _ [i@IUppercase {}]) (ModuleDefinition _ top)) -> do
        local (setModuleName i . resetFlag FTraceInput) $ do
          makeModule pos
          prettyEval top
        return unit
      Module pos (LetPattern _ (ETyped _ (Variable _ [i@IUppercase {}]) (Variable _ sig)) (ModuleDefinition _ top)) -> do
        local (setModuleName i . resetFlag FTraceInput) $ do
          makeModule pos
          prettyEval top
          loadSignature (map stripPosition sig)
        return unit
      ModuleType _ (LetPattern _ (Variable _ [i@IUppercase {}]) (ModuleSignature _ top)) -> do
        modpath <-
          local (setModuleName i) $ getModuleName <&> map (IUppercase npos)
        signatures <- local (setModuleName i) (extractSignatures top)
        lift
          $ modify
          $ over (htypes . modulesignatures) (Map.insert modpath signatures)
        return unit
      Signature {} -> return unit
      SLetIn pos rec defs e -> do
        e' <- eval (ELetIn pos rec defs e)
        t <- typeOf e'
        Context.log $ LogOutput t e'
        return unit
      Typedef _ _ _ cases -> do
        forM_ cases $ \case
          VCanonical _ name ->
            save name (EConstant npos $ CConstructor npos (to name :: Path))
          ConstrOf _ v@(VCanonical _ name) _ -> do
            p <- local (setNameContext CtxLocal) (bind temporaryVariable)
            save name (ELambda npos (Fun1 npos) [p] (EConstructor npos v p))
          Record {} -> return ()
          other -> bad Nothing $ "Cannot evaluate typedef " ++ show other
        return unit
      _ -> bad Nothing $ "Unknown statement " ++ show x
  prettyEval x = do
    Context.log $ LogInput x
    Context.log $ LogInputTree x
    x' <- bind x
    do
      x'' <- resolve x'
      Context.log $ LogInputVersion x''
      x''' <- eval x''
      case x'' of
        Directive2 {} -> return ()
        Module {} -> return ()
        ModuleType {} -> return ()
        _ -> Context.log $ LogOutputTree x'''
      `catchError` appendTrace
                     (ERUnknown (hasPosition x')
                        $ "Cannot evaluate statement " ++ printTree x')
    return ()

instance Evaluable Toplevel where
  eval x = do
    case x of
      Toplevel1 _ s -> mapM_ eval s
      Toplevel2 {} -> undefined
    return unit
  prettyEval x =
    case x of
      Toplevel1 _ s -> mapM_ prettyEval s
      Toplevel2 pos e s -> mapM_ prettyEval (Expression pos e : s)
