--
-- Type reconstruction algorithm.
--

module Types (
  getVarID,
  typeAssign,
  typeBindLabeled,
  typeResolveConstraints,
  typeResolveStatement,
  applyTypeOf,
  pushTypedef,
  -- Module functions.
  typeCheckSignatures,
  typeGetInterface,
  typeUnifySignature,
  -- Trace functions.
  traceModuleDocs,
  traceOutput,
  traceOutputLabeledStatement,
  ) where

import Control.Lens (over, (^.))
import Control.Monad (foldM, forM, forM_, replicateM, unless, when)
import Control.Monad.Except (catchError, ExceptT, runExceptT, throwError)
import Control.Monad.Reader (local)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (modify, get)
import Control.Monad.State.UnificationExtras (localState)
import Control.Unification (UTerm(..), applyBindings, fullprune, freshen, subsumes, unify)
import Control.Unification.IntVar (IntVar(..))
import Control.Unification.Types (BindingMonad(..))
import qualified Data.Bimap as Bimap
import Data.List (any, filter, intercalate, intersect, nub, sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set

import Context
import Flags
import Grammar
import Memory
import PrintOcaml
import TypeConstraint
import StaticTyping
import UtilsOcaml

-- Binding variables.

getRecordID :: IntVar -> [Ident] -> AResult IntVar
getRecordID troot idents = case idents of
  [] -> return troot
  i:tl -> do
    context <- get
    troot2 <- runStaticTypingT $ applyBindings $ UVar troot
    
    -- Find the matching typedef.
    tident <- case getTermIdent troot2 of
      Just tident -> return tident
      Nothing -> head <$> lookupPossibleTypedefs i

    -- Get typedef.
    let Just (STypedef troot' (TypedefRecords fields)) = Map.lookup tident (context ^. htypes . types)

    -- Check that the typedef contains the field.
    TRecordField mutable _ rhs <- case filter (\(TRecordField _ i2 _) -> i == i2) fields of
      [x] -> return x
      _ -> undefined -- It should not occur. It means that (htypes . typedfields) contains bad data.

    -- Assert that the path is mutable.
    scope <- askE
    when (scope ^. ismutable && mutable == MutableNo) $ 
      bad $ "Field '" ++ printTree i ++ "' is not mutable in type '" ++ printTree troot' ++ "'"

    -- Add constraint.
    -- Workaround:
    -- This constraint is processed online to prevent the quadratic number of unifications.
    lhs <- freeVar
    constraint <- typeRecordConstraint [UVar lhs] (UVar troot) [rhs] troot'
    runStaticTypingT $ resolveConstraint $ Debug 3 (printTree idents) constraint

    getRecordID lhs tl

getVarID :: Variable -> AResult IntVar
getVarID v = case v of
  VSimple VBlank -> freeVar -- Freshen blanks ...
  VOp op -> undefined -- It should not occur, after fvBind.
  VSimple (VCanonical name@(CanonicalName index version idents)) | not (null idents) -> do
    troot <- getVarID $ VSimple $ VCanonical $ name {recordidents = []}
    getRecordID troot idents
  _ -> do
    context <- get
    case Bimap.lookup v (context ^. htypes . variables) of
      Nothing -> do 
        fv <- freeVar
        modify $ over (htypes . variables) (Bimap.insert v fv)
        return fv
      Just fv -> do
        scope <- askE
        if isLocalVariable v -- Issue with ad-hoc polymorphism. Ocaml does not support it.
          || scope ^. isnext -- Issue with assigning global signatures.
          || Set.member v (scope ^. recursivefunctions) -- Issue with recursive functions. (let rec = ...)
          then return fv
          else do
            UVar fv' <- runStaticTypingT $ freshen $ UVar fv
            return fv'

typeAssign :: Variable -> Type -> AResult IntVar
typeAssign v t = do
  iv <- getVarID v
  t' <- fromTypeToRelabelledTerm t
  bindVar iv t'
  return iv

typeBindLabeled :: Binder LabeledStatement
typeBindLabeled x = case x of
  SLabeled v e -> do
    IntVar iv <- getVarID v
    return $ SLabeledBound v iv e
  SLabeledTyped v t e -> do
    uv@(UVar (IntVar iv)) <- UVar <$> getVarID v
    let ut = fromTypeToTerm t 
    runStaticTypingT $ uv `unify` ut
    return $ SLabeledBound v iv e
  SLabeledBound {} -> badInternal 3 "Do not use typeBindLabeled twice..."

typeConstructorParameters :: Constructor -> [Type]
typeConstructorParameters x = case x of
  ConstructorEps ident -> []
  ConstructorType ident t -> unwrapTTuple $ typeFlatten t
  -- ConstructorAs {} -> undefined -- TODO
  where
    unwrapTTuple :: Type -> [Type]
    unwrapTTuple x = case x of
      TTuple xs -> xs
      _ -> [x]

resolveConstraint :: DebugWrapper TypeConstraint -> StaticTypingT ()
resolveConstraint d@(Debug case_ msg x) = case x of
  TEquals t1 t2 -> do {
    t1 `unify` t2;
    return ()
    } `catchError` (\e' -> lift $ do {
      traceTypeInfo;
      let e = ERUnknown $ "resolveConstraint (equals case=" ++ show case_  ++ "): " ++ show x ++ "\n context=" ++ msg in
        appendSomeTrace e e'
    })
  TSubsumes t1 t2 -> do
    lift $ pushToOstream $ show (t1, t2)
    success <- localState $ t1 `subsumes` t2
    unless success $ lift $ do {
      traceTypeInfo;
      -- TODO remove useless logs..
      context <- get;
      forM_ (Map.toList (context ^. htypes . assignments)) $ \(v, t) -> do {
        t' <- fullprune t;
        pushToOstream $ show (v, t');
      };
      t1' <- fullprune t1;
      t2' <- fullprune t2;
      pushToOstream $ show t1';
      pushToOstream $ show t2';
      bad $ "resolveConstraint (subsumes case=" ++ show case_ ++ "): MismatchFailure " ++ show (t1, t2)
    }

typeResolveConstraints :: Result
typeResolveConstraints = do
  context <- get
  forM_ (context^.htypes.constraints) $ \x -> do
    pushToOstreamFlagged FTraceTypeConstraints $ printTree x
    runStaticTypingT $ resolveConstraint x
  -- traceTypesAsGraphViz (context ^. htypes . constraints)
  popConstraints

typeResolveStatement :: Statement -> AResult (Maybe Type)
typeResolveStatement x = do
  t <- typeFindConstraints x
  typeResolveConstraints
  case t of
    Just x -> do
      t' <- runStaticTypingT $ fromTermToType x
      return . Just . typeSimplify $ t'
    Nothing -> return Nothing

class (Print a, Show a) => Typeable a where
  typePatternMatch :: a -> AResult OpenTerm
  typePatternMatch x = bad $ "typePatternMatch: Not matchable... " ++ show x
  typeFindConstraints :: a -> AResult (Maybe OpenTerm)
  typeFindConstraints x = return Nothing -- Default constraint
  typeJustFindConstraints :: a -> AResult OpenTerm
  typeJustFindConstraints x = do
    t <- typeFindConstraints x
    case t of
      Nothing -> 
        bad $ "typeFindConstraints failed for the following expression :\n" ++ show x
      Just t' ->
        return t'

instance Typeable SequencingExpression where
  typeFindConstraints x = case x of
    ESeq expression -> 
      typeFindConstraints expression
    EAssign variable expression -> do
      t <- local setMutable (UVar <$> getVarID variable)
      t' <- typeJustFindConstraints expression
      pushConstraint $ Debug 10 (printTree x) $ TEquals t t'
      return $ Just $ UTerm $ TermT1Const TUnit

-- Fields -> Result -> Fields -> Result -> Constraint
typeRecordConstraint :: [OpenTerm] -> OpenTerm -> [Type] -> Type -> AResult TypeConstraint
typeRecordConstraint lhs lres rhs rres = do
  let lfun = termComposeFunction $ lhs ++ [lres]
  rfun <- fromTypeToRelabelledTerm $ typeComposeFunction $ rhs ++ [rres]
  return $ TEquals lfun rfun

typeFindConstraintsRecord :: [(Ident, OpenTerm)] -> AResult (Maybe OpenTerm)
typeFindConstraintsRecord params = do
  -- Find all records that contain exactly those fields.
  -- Intersect over possible record names.
  let name_head:name_tail = map fst params
  acc <- lookupPossibleTypedefs name_head
  possible_typedefs <- foldM 
    (\acc f -> intersect acc <$> lookupPossibleTypedefs f) acc name_tail

  -- All fields must occur exactly once.
  context <- get
  signature <- foldM (\acc ident -> case acc of 
    Nothing -> case Map.lookup ident (context ^. htypes . types) of
      Just x@(STypedef _ (TypedefRecords fields)) | length fields == length params -> 
        return $ Just x
      _ -> return Nothing
    Just r -> return $ Just r) Nothing possible_typedefs

  signature@(STypedef rres (TypedefRecords fields)) <- case signature of
    Nothing -> bad $ "Missing record type signature with record fields : " ++ intercalate ", " (map printTree (name_head:name_tail))
    Just r -> return r

  -- Alphabetically ordered, dummy constructor.
  let sortByIdents = sortBy (\(Ident i, _) (Ident j, _) -> compare i j)
  let lhs = map snd . sortByIdents $ params
  let rhs = map snd . sortByIdents . map (\(TRecordField m i t) -> (i, t)) $ fields

  lres <- UVar <$> freeVar
  constraint <- typeRecordConstraint lhs lres rhs rres
  pushConstraint $ Debug 22 (printTree signature) constraint

  return $ Just lres

instance Typeable Expression where 
  typeFindConstraints x = case x of
    EConst constant -> typeFindConstraints constant
    EString str -> return . Just . UTerm . TermT1Const $ TString
    EVar variable -> typeFindConstraints variable
    EReferenceMemory ptr -> undefined
    EOp1 prefixop expression -> do
      t <- typeJustFindConstraints expression
      return . Just $ t
    EOp2 expression1 infixop expression2 -> do
      [t1, t2] <- forM [expression1, expression2] typeJustFindConstraints
      pushConstraint $ Debug 2 (printTree x) $ TEquals t1 t2
      return . Just $ t1
    ERecord lstmts -> do
      params <- forM lstmts $ \(SLabeled (VSimple (VVariable [i])) e) -> do
        t <- typeJustFindConstraints e
        return (i, t)
      typeFindConstraintsRecord params
    EIfThenElse expression1 expression2 expression3 -> do
      [t1, t2, t3] <- forM [expression1, expression2, expression3] typeJustFindConstraints
      pushConstraint $ Debug 7 (printTree x) $ TEquals t1 (UTerm $ TermT1Const TBool)
      pushConstraint $ Debug 4 (printTree x) $ TEquals t2 t3
      return . Just $ t2
    ETypeOf expression -> do
      typeFindConstraints expression
      return . Just . UTerm . TermT1Const $ TString
    EMatch simplevariables matchings -> do
      svs <- forM simplevariables $ typeJustFindConstraints . VSimple
      uv <- UVar <$> freeVar
      let temp = termComposeFunction $ svs ++ [uv]
      let n = length simplevariables
      forM_ matchings $ \m@(Matching1 patterns expr) -> case patterns of
        [EVar (VSimple VBlank)] -> do -- Matching blank
          tres <- typeJustFindConstraints expr
          pushConstraint $ Debug 5 (printTree $ EMatch simplevariables [m]) $ TEquals uv tres
        _ | length patterns == n -> do
          t <- typeJustFindConstraints m
          pushConstraint $ Debug 6 (printTree $ EMatch simplevariables [m]) $ TEquals temp t
        _ -> bad "Invalid number of parameters in the match statement."
      return . Just $ uv
    ELocalDefinition valuedefinition expression -> do
      typeFindConstraints valuedefinition
      t <- typeJustFindConstraints expression
      return $ Just t
    EStack seq -> do
      ts <- forM seq typeFindConstraints
      return $ last ts
    EFor iterator expr_start _ expr_end expressions -> do
      [t1, t2, t3] <- forM [expr_start, expr_end, EStack expressions] typeJustFindConstraints
      let tint = UTerm $ TermT1Const TInt
      let tunit = UTerm $ TermT1Const TUnit
      pushConstraint $ Debug 21 (printTree iterator) $ TEquals t1 tint
      pushConstraint $ Debug 16 (printTree expr_start) $ TEquals t1 tint
      pushConstraint $ Debug 17 (printTree expr_end) $ TEquals t2 tint
      pushConstraint $ Debug 18 (printTree expressions) $ TEquals t3 tunit
      return $ Just tunit
    EWhile condition expressions -> do
      let tbool = UTerm $ TermT1Const TBool
      let tunit = UTerm $ TermT1Const TUnit
      [t1, t2] <- forM [condition, EStack expressions] typeJustFindConstraints
      pushConstraint $ Debug 19 (printTree condition) $ TEquals t1 tbool
      pushConstraint $ Debug 20 (printTree expressions) $ TEquals t2 tunit
      return $ Just tunit
    EFunctionCall variable expressions -> case variable of
      VOpTuple T -> typeFindConstraints (ETuple expressions)
      _ -> do
        t <- UVar <$> getVarID variable
        ts <- forM expressions typeJustFindConstraints
        res <- UVar <$> freeVar
        let uv = termComposeFunction $ ts ++ [res]
        pushConstraint $ Debug 11 (printTree x) $ TEquals t uv
        return . Just $ res
    ETuple expressions -> do
      res <- freeVar
      ts <- forM expressions typeJustFindConstraints
      bindVar res (termComposeTuple ts)
      return . Just . UVar $ res
    ENamedTuple name expressions -> undefined
    ERecursiveFunctionCall variable expressions expression -> do
      constructor_term <- typeJustFindConstraints variable
      parameter_terms@(h:xs) <- forM expressions typeJustFindConstraints
      guard_term <- typeJustFindConstraints expression
      -- 1. Unify parameter_types
      forM_ xs $ \x ->
        pushConstraint $ Debug 12 (printTree [h, x]) $ TEquals h x
      -- 2. Unify with last constructor variable(h, expression)
      let final_term = termComposeFunction [h, guard_term, guard_term]
      pushConstraint
        $ Debug 13 (printTree variable ++ "(" ++ printTree h ++ "," ++ printTree guard_term ++ ")")
        $ TEquals constructor_term final_term
      return . Just $ guard_term
    ELambda functionprefix matching -> typeFindConstraints matching
    ENull -> undefined
    ERaise exceptionname typeableexpression -> do
      iv <- freeVar
      _ <- typeJustFindConstraints typeableexpression
      return $ Just $ UVar iv
    _ -> error (show x) -- TODO remove

  typePatternMatch x = case x of
    EConst constant -> 
      typeJustFindConstraints constant
    EString str ->
      return . UTerm . TermT1Const $ TString
    EVar variable ->
      UVar <$> getVarID variable
    ERecord lstmts -> typeJustFindConstraints x
    EFunctionCall variable expressions -> undefined -- This should never happen.
    ETuple expressions -> do
      uv <- UVar <$> freeVar
      ts <- forM expressions typePatternMatch
      pushConstraint $ Debug 14 (printTree x) $ TEquals uv (termComposeTuple ts)
      return uv
    ENamedTuple name expressions -> do
      t <- typeJustFindConstraints $ VConstructor name
      ts <- forM expressions typePatternMatch
      res <- UVar <$> freeVar
      let function_term = termComposeFunction $ ts ++ [res]
      pushConstraint $ Debug 15 (printTree x) $ TEquals t function_term
      return res
    ERecursiveFunctionCall variable expressions expression -> undefined -- Unimplemented?
    _ -> bad "Type not supported in pattern matching"

instance Typeable LabeledStatement where
  typeFindConstraints e = undefined -- Code for typing records?

instance Typeable Constant where
  typeFindConstraints x = do
    t' <- fromTypeToRelabelledTerm $ typeConstant x
    return . Just $ t'

instance Typeable Variable where
  typeFindConstraints x = Just . UVar <$> getVarID x

instance Typeable Matching where
  typeFindConstraints e = case e of
    Matching1 expressions expression -> do
      ts <- forM expressions typePatternMatch
      t <- typeJustFindConstraints expression
      return $ Just $ termComposeFunction $ ts ++ [t]

instance Typeable Definition where
  typeFindConstraints x = case x of
    SValue _rec (name:params) expr -> do
      alias <- local setNext $ typeJustFindConstraints name
      types <- forM params typeJustFindConstraints -- It may be empty.
      expression_type <- local (setRecursiveFunction _rec name) $ typeJustFindConstraints expr
      let f_type = termComposeFunction $ types ++ [expression_type]
      pushConstraint $ Debug 8 (printTree x) $ TEquals alias f_type
      return $ Just alias
    SExternal v type_ _ -> do
      t <- typeJustFindConstraints v
      t' <- fromTypeToRelabelledTerm $ typeFlatten type_
      pushConstraint $ Debug 1 (printTree x) $ TEquals t t'
      return $ Just t

instance Typeable ModuleContent where
  typeFindConstraints x = case x of
    SExceptionDefinition name type_ ->
      return Nothing -- TODO
    SDefinition definition ->
      typeFindConstraints definition

instance Typeable Statement where
  typeFindConstraints x = case x of
    SExpression expression -> 
      typeFindConstraints expression
    SExpressionWithType expression type_ -> do
      t <- typeJustFindConstraints expression
      type_' <- fromTypeToRelabelledTerm $ typeFlatten type_
      pushConstraint $ Debug 9 (printTree x) $ TSubsumes t type_'
      return $ Just t
    SModuleContent modulecontent ->
      typeFindConstraints modulecontent
    SModuleDefinition smodule modulecontents -> do
      forM_ modulecontents $ \m -> do 
        typeFindConstraints m
        -- Issue. Freshen has to be called after full type resolution of dependent functions.
        typeResolveConstraints
      return Nothing
    _ -> return Nothing

-- Utils

fromTypeToRelabelledTerm :: Type -> AResult OpenTerm
fromTypeToRelabelledTerm x = 
  runStaticTypingT $ freshen (fromTypeToTerm x)

applyTypeOf :: Expression -> AResult Expression
applyTypeOf expression = case expression of
  EVar variable -> do
    t <- peekType variable
    return . EString . printTree $ t
  _ -> bad $ "Expected a variable. __typeof__ is not applicable to " ++ printTree expression

peekType :: Variable -> AResult Type
peekType variable = do
  -- Hacky. setNext ensures that a new copy is not created.
  uv <- local setNext $ UVar <$> getVarID variable
  t <- runStaticTypingT $ fromTermToType uv
  return $ typeSimplify t

-- It returns all records that contain such field.
lookupPossibleTypedefs :: Ident -> AResult [Ident]
lookupPossibleTypedefs x = do
  context <- get
  case Map.lookup x (context ^. htypes . typedfields) of
    Nothing -> bad $ "There is no record that contains field with name '" ++ printTree x ++ "'."
    Just r -> return r

pushTypedef :: Statement -> Result
pushTypedef x = case x of
  STypedef type_ definition -> do
    context <- get  
    let type_' = typeFlatten type_
    let Just ident = getTypeIdent type_'
    if Map.member ident $ context ^. htypes . types
      then bad $ printTree ident ++ " is already defined."
      else do
        -- Save signature.
        let x' = STypedef type_' definition
        modify $ over (htypes.types) (Map.insert ident x')

        case definition of
          TypedefConstructors constructors ->
            forM_ constructors $ \c -> do
              let name = c ^. lensNameFromConstructor
              pushConstructor name ident
              -- Push constructor type.
              let params = typeConstructorParameters c
              let constructor_type = typeComposeFunction $ params ++ [type_']
              typeAssign (VConstructor [name]) constructor_type
          TypedefRecords fields ->
            -- Save inverse relation (field -> type alias)
            forM_ fields $ \f@(TRecordField mutable name _) ->
              modify $ over (htypes.typedfields) (Map.alter (maybePrepend ident) name)
  _ -> bad "pushType should be called only with STypedef"

-- Module functions.

typeCheckSignatures :: Maybe [(Ident, Type)] -> Map.Map Ident OpenTerm -> Result
typeCheckSignatures interface definitions = case interface of
  Nothing -> return ()
  Just signatures1 ->
    forM_ signatures1 $ \(ident, type_) -> case Map.lookup ident definitions of
      Nothing -> bad $ "Element '" ++ printTree ident ++ "' is required by the module signature."
                      ++ "\n Expected type: " ++ printTree type_ 
      Just v -> do
        v' <- runStaticTypingT $ fromTermToType v
        let [type_', v''] = typeSimplify <$> [type_, v']
        unless (type_' == v'') $
          bad $ "Type of '" ++ printTree ident ++ "' does not match."
              ++ "\n Signature: " ++ show v''
              ++ "\n Definition: " ++ show type_' -- [TODO] Error messages should be somewhere else

typeGetInterface :: ModuleName -> AResult (Maybe [(Ident, Type)])
typeGetInterface smodule = case smodule of
  SModuleName0 modname -> return Nothing
  SModuleName1 modname signame -> do
    context <- get
    case Map.lookup signame (context ^. hmodules . signatures) of
      Nothing -> bad $ "Module signature with name '" ++ printTree signame ++ "' does not exist."
      Just signatures1 -> return $ Just $ map (\(Signature1 (VSimple (VVariable [i])) t) -> (i, t)) signatures1

typeUnifySignature :: Int -> CanonicalName -> Type -> Result
typeUnifySignature lhs name rhs = do
  let lhs' = UVar $ IntVar lhs
  rhs' <- fromTypeToRelabelledTerm rhs
  pushConstraint $ Debug 24 (printTree name) $ TEquals lhs' rhs'

-- Trace functions.
traceTypeInfo :: Result
traceTypeInfo = do
  context <- get
  pushToOstream "typeInfo:"
  forM_ (Bimap.toList (context ^. htypes . variables)) $ \(v, t) -> do
    t' <- fullprune (UVar t)
    pushToOstream $ "  " ++ printTree v ++ " has type " ++ show t 
    unless (termChanged t t') $ pushToOstream $ "    pruned to " ++ show t'
    where
      termChanged :: IntVar -> OpenTerm -> Bool
      termChanged t x = case x of
        UTerm {} -> False
        UVar x' -> t == x'

traceOutput :: (Print a, Typeable a) => AResult a -> AResult a
traceOutput stmt = do
  res <- local setNoOutput stmt
  scope <- askE
  unless (scope^.islocal) $ do
    context <- get
    pushToOstream $ " value: " ++ printTree res
    pushToOstreamFlagged FTraceOutputTree $ "output: " ++ show res
  return res

traceOutputLabeledStatement :: LabeledStatement -> AResult Expression
traceOutputLabeledStatement (SLabeledBound v _ e) =
  traceOutput $ return e

-- ModuleDocs

peekModuleDocs :: CanonicalName -> AResult ModuleDocs
peekModuleDocs x@(CanonicalModule name) = do
  proot <- memRoot
  ptr <- memChangeDirectory proot (fvPath x)
  case ptr of
    Nothing -> bad "Invalid directory."
    Just p -> do
      Just (ERecordMemory fields) <- memRead p
      docs <- forM fields $ \(ident, ptr) -> do
        Just (ERecordMemory versions) <- memRead ptr
        forM versions $ \(version, ptr) ->
          -- Find type... [TODO]
          -- SLabeledBound v' i' e' -> do
          -- let extract (VSimple (VVariable vs)) = vs
          -- let name = concatMap extract [v, v']
          -- let uv = UVar . IntVar $ i'
          -- t <- runStaticTypingT $ fromTermToType uv
          -- return $ ModuleDocsSignature name (typeSimplify t)
          return $ ModuleDocsSignature [ident, version] (typeNth 1)
      return $ ModuleDocs x $ concat docs
  
traceModuleDocs :: CanonicalName -> Result
traceModuleDocs x = do
  docs <- peekModuleDocs x
  pushToOstream . printTree $ docs
