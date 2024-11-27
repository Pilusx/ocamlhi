module StaticBinding (
  fvBind,
  fvUnbind,
  toLabeledStatementFromModuleContent,
  toLabeledStatementFromDefinition
  ) where

import Control.Lens ((^.), over)
import Control.Monad (forM)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (local)
import Control.Monad.Trans.State (modify, get)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)

import Context
import Grammar
import Memory
import PrintOcaml
import UtilsOcaml

-- Binding names.

-- Checks whether the given field is a private module member.
fvIsPrivate :: (Ident, NName) -> AResult Bool
fvIsPrivate (modname, fields) = case fields of
  [] -> return False
  (i:_) -> do
    context <- get
    case Map.lookup modname (context ^. hmodules . implements) of
      Nothing -> return False
      Just signame -> case Map.lookup signame (context ^. hmodules . signatures) of
        Nothing -> 
          badInternal 5 $ "There is no module signature with given name: " ++ printTree signame
        Just sigs -> 
          return . not . any (\(Signature1 (VSimple (VVariable [i2])) _) -> i == i2) $ sigs

fvCanonicalize :: NName -> AResult (Ident, NName)
fvCanonicalize name@(x:xs) = do
  context <- get
  modname <- fvLastModule x
  scope <- askE
  case modname of
    Just m | scope ^. modulename == modname -> return (m, xs) -- We are in the same module.
    Just m -> do
      b <- fvIsPrivate (m, xs)
      if b then bad "Private field." else return (m, xs)
    Nothing | scope ^. islocal -> return (Ident ".local", name)
    _ -> case scope ^. modulename of
      Nothing -> return (Ident ".global", name)
      Just m -> return (m, name)

fvUnbind :: CanonicalIndex -> Result
fvUnbind cindex =
  modify $ over hversionmanager (Map.alter (\(Just x) -> Just $ x-1) cindex)  

fvNext :: (Ident, NName) -> AResult CanonicalName
fvNext (modname, names) = do
  let index = (modname, head names)

  -- Increment version.
  modify $ over hversionmanager (Map.alter maybeIncrement index)

  context <- get
  case Map.lookup index (context ^. hversionmanager) of
    Just v -> return $ CanonicalName {
        cindex = index,
        version = v,
        recordidents = tail names
      }
    _ -> undefined

fvLast :: (Ident, NName) -> AResult (Maybe CanonicalName)
fvLast (modname, names) = do
  let index = (modname, head names)
  context <- get
  scope <- askE

  let indexer v = return $ Just CanonicalName {
      cindex = index,
      version = v,
      recordidents = tail names
    }
  
  if scope ^. islocal
    -- We have found a local variable.
    then case Map.lookup index (scope ^. localversionmanager) of 
      Just v -> indexer v
      Nothing -> return Nothing
    -- We are looking for the last global variable.
    else case Map.lookup index (context ^. hversionmanager) of 
      Just v -> indexer v
      Nothing -> return Nothing

-- Binding modules.

fvModuleIndexer :: Ident -> Int -> Ident
fvModuleIndexer (Ident i) v = Ident $ i ++ "." ++ show v 

fvNextModule :: Ident -> AResult Ident
fvNextModule modname = do
  modify $ over (hmodules . lastindex) (Map.alter maybeIncrement modname)
  Just modname' <- fvLastModule modname
  return modname'

fvLastModule :: Ident -> AResult (Maybe Ident)
fvLastModule modname@(Ident i) = do
  context <- get
  case Map.lookup modname (context ^. hmodules . lastindex) of
    Nothing -> return Nothing
    Just index -> return . Just $ fvModuleIndexer modname index

fvLastModuleDocs :: (Ident, NName) -> AResult (Maybe CanonicalName)
fvLastModuleDocs (modname, names) = return $ case names of
  [] -> Just $ CanonicalModule modname
  _ -> Nothing

-- Local bindings.

fvInsert :: Variable -> ScopeM -> ScopeM
fvInsert v = case v of
  VSimple (VCanonical (CanonicalName index version _)) ->
    pushScopedVariableVersion index version
  VSimple VBlank -> id
  _ -> undefined

fvInsertN :: [Variable] -> ScopeM -> ScopeM
fvInsertN vs = compose (map fvInsert vs)

fvPushMatching :: Expression -> (ScopeM -> ScopeM)
fvPushMatching x = case x of
  ERecord lstmts ->
    compose $ map (\(SLabeled v e) -> fvPushMatching e) lstmts
  ETuple exprs ->
    compose $ map fvPushMatching exprs
  ENamedTuple name exprs ->
    compose $ map fvPushMatching exprs
  EVar pv -> case pv of
    VSimple VBlank -> id
    VSimple _ -> fvInsert pv
    VConstructor _ -> id
    _ -> throwBadMatching (-1) $ show pv
  EConst _  -> id
  _ -> throwBadMatching (-2) $ show x


class StaticBinding a where
  fvBind :: Binder a
  fvBind = return

instance StaticBinding a => StaticBinding [a] where
  fvBind = mapM fvBind

instance StaticBinding ExceptionName
instance StaticBinding FunctionPrefix
instance StaticBinding Type

instance StaticBinding SequencingExpression where
  fvBind x = case x of
    ESeq exprs -> do
      exprs' <- fvBind exprs
      return $ ESeq exprs'
    EAssign v e -> do
      v' <- fvBind v
      e' <- fvBind e
      return $ EAssign v' e'

fvBindOp :: String -> AResult Variable
fvBindOp name =
  fvBind $ VSimple $ VVariable [Ident $ "(" ++ name ++ ")"]

instance StaticBinding Expression where
  fvBind x = case x of
    EConst _ -> return x
    EString _ -> return x
    EVar v -> EVar <$> fvBind v
    EReferenceMemory _ -> return x
    EOp1 op e1 -> do
      e1' <- fvBind e1
      v' <- fvBindOp $ case op of
        -- Internally ops in -3 and (3 - 3) have to be distinguishable..
        PrefixOpMinus ->  "~" ++ printTree op
        _ -> printTree op 
      return $ EFunctionCall v' [e1']
    EOp2 e1 op e2 -> do
      [e1', e2'] <- forM  [e1, e2] fvBind
      v' <- fvBindOp $ printTree op
      return $ EFunctionCall v' [e1', e2']
    ERecord lstmts -> ERecord <$> fvBind lstmts
    ERecordMemory _ -> return x
    EIfThenElse e1 e2 e3 -> EIfThenElse <$> fvBind e1 <*> fvBind e2 <*> fvBind e3
    EMatch simplevariables matchings -> do
      sv <- fvBind simplevariables
      ms <- local (fvInsertN (map VSimple sv)) (fvBind matchings)
      return $ EMatch sv ms
    ETypeOf e -> ETypeOf <$> fvBind e
    ELocalDefinition vd e -> do
      vd'@(SValue _ (name':_) _) <- local setLocal (fvBind vd)
      e' <- local (fvInsert name') (fvBind e)
      return $ ELocalDefinition vd' e'
    EStack exprs -> EStack <$> fvBind exprs
    EFor variable expr_start op expr_end expr_loop -> do
      v' <- local (setLocal . setNext) (fvBind variable)
      -- [TODO] Assert that there are no record idents in v', and that v' is not a constructor.
      e_start' <- fvBind expr_start
      e_end' <- fvBind expr_end
      e_loop' <- local (fvInsert v') (fvBind expr_loop)
      return $ EFor v' e_start' op e_end' e_loop'
    EWhile e exprs -> EWhile <$> fvBind e <*> fvBind exprs
    EFunctionCall v exprs -> EFunctionCall <$> fvBind v <*> fvBind exprs
    EExternalFunctionCall _ _ -> return x
    ETuple exprs -> ETuple <$> fvBind exprs
    ENamedTuple name exprs -> ENamedTuple name <$> fvBind exprs
    ERecursiveFunctionCall v exprs e -> ERecursiveFunctionCall <$> fvBind v <*> fvBind exprs <*> fvBind e
    ELambda prefix matching -> ELambda prefix <$> fvBind matching
    ERaise name e -> ERaise <$> fvBind name <*> fvBind e
    ENull -> return x

instance StaticBinding LabeledStatement where
  fvBind x = case x of
    SLabeled variable expression -> do
      e <- fvBind expression
      return $ SLabeled variable e
    SLabeledTyped variable type_ expression -> do
      e <- fvBind expression
      let type_' = typeFlatten type_
      return $ SLabeledTyped variable type_' e

fvAlternative :: [AResult a] -> AResult a
fvAlternative (x:xs) =
  case xs of 
    [] -> x
    _ -> x `catchError` \_ -> fvAlternative xs

fvFromJust :: Maybe a -> AResult a
fvFromJust x = case x of
  Just t -> return t
  Nothing -> bad "Error"

instance StaticBinding SimpleVariable where
  fvBind x = case x of
    VVariable names -> do
      scope <- askE
      VCanonical <$> if scope^.isnext
        then fvCanonicalize names >>= fvNext
        else fvAlternative [
          -- Check if it is module docs.
          fvCanonicalize names >>= fvLastModuleDocs >>= fvFromJust,
          -- Check locally.
          local setLocal (fvCanonicalize names >>= fvLast >>= fvFromJust),
          -- Check current module, when in module.
          local setGlobal (fvCanonicalize names >>= fvLast >>= fvFromJust), 
          -- Check globally.
          local (setGlobal . resetModuleName) (fvCanonicalize names >>= fvLast >>= fvFromJust),
          -- Print error..
          bad $ "fvLast: Variable not found " ++ show x
          ]
    VBlank -> bad "Blank is not convertible."
    VCanonical name -> bad "Double binding is not permitted."

instance StaticBinding Variable where
  fvBind x = case x of
    VSimple simplevariable -> case simplevariable of
      VVariable name -> do
        scope <- askE
        b <- isConstructor name
        if b
          then return . VConstructor $ name
          else do
            p <- checkScopedVariable name
            VSimple <$> case p of
              Just _ -> local setLocal (fvBind simplevariable)
              Nothing -> fvBind simplevariable
      _ -> return x
    VOpTuple tupleconstr -> return x
    VOp op -> fvBindOp $ printTree op

-- Buggy grammar ... This should not exist...
fvFixGrammar :: Expression -> AResult Expression
fvFixGrammar x = case x of
  EFunctionCall name es -> case name of
    VOpTuple T -> return $ ETuple es
    VConstructor x -> do 
      es' <- forM es fvFixGrammar
      return $ ENamedTuple x es'
  _ -> return x

instance StaticBinding Matching where
  fvBind x = case x of
    Matching1 patterns expression -> do
      ps <- local (setLocal . setNext) (forM patterns fvBind)
      ps' <- forM ps fvFixGrammar
      e <- local (compose $ map fvPushMatching ps') (fvBind expression)
      return $ Matching1 ps' e

instance StaticBinding Definition where
  fvBind x = case x of
    SValue letrec (name:params) expr -> case letrec of
      RecNo -> do
        params' <- local (setLocal . setNext) (fvBind params)
        expr' <- local (fvInsertN params') (fvBind expr)
        name' <- local setNext (fvBind name)
        return $ SValue letrec (name':params') expr'
      RecYes -> do
        name' <- local setNext (fvBind name)
        params' <- local (setLocal . setNext) (fvBind params)
        let names' = name':params'
        expr' <- local (fvInsertN names') (fvBind expr)
        return $ SValue letrec names' expr'
    SExternal variable type_ binding -> do
      v' <- local setNext (fvBind variable)
      return $ SExternal v' type_ binding

instance StaticBinding ModuleContent where
  fvBind x = case x of
    SDefinition valuedefinition -> do
      vd <- fvBind valuedefinition
      return $ SDefinition vd
    _ -> return x

instance StaticBinding ModuleName where
  fvBind x = case x of
    SModuleName0 name -> SModuleName0 <$> fvNextModule name
    SModuleName1 name signame -> do
      name' <- fvNextModule name
      msigname <- fvLastModule signame
      case msigname of
        Just signame' -> do
          context <- get
          if Map.member signame' (context ^. hmodules . signatures)
            then do
              modify $ over (hmodules . implements) (Map.insert name' signame')
              return $ SModuleName1 name' signame'
            else bad $ "Expected a valid module signature. Got '" ++ printTree x ++ "'."
        _ -> 
          bad $ "Signature with name '" ++ printTree signame ++ "' does not exist."

instance StaticBinding Statement where
  fvBind x = case x of
    SDirective _ -> return x
    SEnable _ -> return x
    SExpression e -> SExpression <$> fvBind e
    SExpressionWithType e t -> SExpressionWithType <$> fvBind e <*> fvBind t
    SSignature _ -> return x
    STypedef type_ definition -> do
      definition' <- case definition of
        TypedefConstructors constructors -> return definition
        TypedefRecords fields -> do
          fields' <- forM fields $ \(TRecordField m i t) -> 
            return $ TRecordField m i (typeFlatten t)
          return $ TypedefRecords fields'
      return $ STypedef (typeFlatten type_) definition'
    SModuleContent c -> SModuleContent <$> fvBind c
    SModuleSignature name sigs -> do
      name' <- fvNextModule name
      return $ SModuleSignature name' sigs
    SModuleDefinition smodule modulecontents -> do
      smodule' <- fvBind smodule
      let name = smodule' ^. lensNameFromModuleName
      contents <- local (setNoOutput . setModuleName name) $ forM modulecontents fvBind
      return $ SModuleDefinition smodule' contents

-- TODO merge Definition with ModuleContent

toLabeledStatementFromDefinition :: Definition -> AResult LabeledStatement
toLabeledStatementFromDefinition x = case x of
  SValue _ (name:params) expression -> do
    mparams <- local setLocal (forM params $ return . EVar)
    return $ SLabeled name $ toLambda mparams expression
  SExternal variable type_ binding -> do
    let count = typeCountParameters $ typeFlatten type_
    params <- forM (toParameters count) $ \p -> local (setLocal . setNext) (fvBind p)
    let expr = toLambda params $ EExternalFunctionCall binding params
    return $ SLabeled variable expr
      where
        typeCountParameters :: Type -> Int -- [TODO] This is not ideal ....
        typeCountParameters x = case x of
          TFunction x1 x2 -> 1 + typeCountParameters x2 
          _ -> 0

toLabeledStatementFromModuleContent :: ModuleContent -> AResult LabeledStatement
toLabeledStatementFromModuleContent x = case x of
  SExceptionDefinition (NException (Ident ename)) type_ -> do
    name <- VSimple . VCanonical <$> (fvCanonicalize [Ident ename] >>= fvNext) -- ?
    let expr = EString ename
    return $ SLabeled name expr -- [TODO] WTF? Type checking for exceptions ...

  SDefinition lvd -> toLabeledStatementFromDefinition lvd
