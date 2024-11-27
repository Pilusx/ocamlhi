--
-- This file contains the main represenation of the interpreter's state (Context).
-- It provides API for state management and error handling.
--

{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             TemplateHaskell #-}

module Context where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Lens (over, (^.), makeLenses)
import Control.Monad (forM_, forM, mapM_, when)
import Control.Monad.Except (runExceptT, ExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, asks, local)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (runStateT, StateT, get, modify)
import Control.Unification.IntVar (IntVar(..))
import Control.Unification (UTerm(..))
import qualified Data.Bimap as Bimap
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import ErrM
import Flags
import Grammar
import LexOcaml (Token)
import Memory
import PrintOcaml
import TypeConstraint
import UtilsOcaml

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

type VersionManager = Map.Map CanonicalIndex Int

data Memory = Memory {
  _rootptr :: Ptr,
  _nextptr :: Ptr,
  _memory :: Map.Map Ptr Expression,
  _counters :: Map.Map Ptr Int
}

data TypeHandler = TypeHandler {
  _types :: Map.Map Ident Statement, -- (STypedef) `a list = Nil | Node `a * `a list
  _typedconstructors :: Map.Map Ident Ident, -- Node ... -> list
  _typedfields :: Map.Map Ident [Ident], -- All record types that contain such field.

  -- Unification
  _nextfresh :: Int,
  _constraints :: [DebugWrapper TypeConstraint],
  _variables :: Bimap.Bimap Variable IntVar,
  _assignments :: Map.Map IntVar OpenTerm
}

data ModuleHandler = ModuleHandler {
  _signatures :: Map.Map Ident [Signature], -- module type X = sig ... end
  _lastindex :: Map.Map Ident Int, -- Keeps the id of the last defined module.
  _implements :: Map.Map Ident Ident -- module X_impl : X = struct ... end
}

data Context = Context {
  _hglobals :: Memory,
  _hversionmanager :: VersionManager,
  _htypes :: TypeHandler,
  _hmodules :: ModuleHandler,
  _hflags :: Set.Set Flag
}

data Scope = Scope {
  _hlocals :: Map.Map NName Ptr,
  _localversionmanager :: VersionManager,
  _islocal :: Bool,
  _isnext :: Bool,
  _ismutable :: Bool,
  _isprinting :: Bool,
  _modulename :: Maybe Ident,
  _recursivefunctions :: Set.Set Variable -- let rec f ... = ... f(y)
}

$(makeLenses ''Memory)
$(makeLenses ''TypeHandler)
$(makeLenses ''ModuleHandler)
$(makeLenses ''Context)
$(makeLenses ''Scope)

type ScopeM = Err Scope
type EEnvironment = ExceptT EError IO
type REnvironment = ReaderT ScopeM EEnvironment
type AResult = StateT Context REnvironment
type Result = AResult ()
type Binder a = a -> AResult a

-- Flags

flagEnable :: String -> Result
flagEnable f = do
  let flag = readFlag f
  modify $ over hflags (Set.insert flag)
  pushToOstream $ "enable: flag " ++ f

-- Lifting

liftIO :: IO a -> AResult a
liftIO = lift . lift . lift

-- Scoped variables

setNoOutput :: ScopeM -> ScopeM
setNoOutput scope = over isprinting (const False) <$> scope

setLocal, setGlobal :: ScopeM -> ScopeM
setLocal scope = over islocal (const True) <$> scope
setGlobal scope = over islocal (const False) <$> scope

setNext :: ScopeM -> ScopeM
setNext scope = over isnext (const True) <$> scope

setMutable :: ScopeM -> ScopeM
setMutable scope = over ismutable (const True) <$> scope

setModuleName :: Ident -> ScopeM -> ScopeM
setModuleName name scope = over modulename (const $ Just name) <$> scope

resetModuleName :: ScopeM -> ScopeM
resetModuleName scope = over modulename (const Nothing) <$> scope

setRecursiveFunction :: RecursivePrefix -> Variable -> (ScopeM -> ScopeM)
setRecursiveFunction _rec v scope = case v of
  VSimple (VCanonical name) | _rec == RecYes -> 
    over recursivefunctions (Set.insert v) <$> scope
  _ -> scope

-- Error handling

data EError
  = ERBind Variable
  | ERInternal Int String
  | ERException String String
  | ERNoMatching Expression
  | ERParseFailed Bool FilePath [Token] String
  | ERUnknown String
  | EROperator String [Expression]
  | ERMatchingFailed
  | ERPushMatching Int String
  | ERStackTrace [EError]

instance Show EError where
  show x = case x of
    ERBind v -> "[EBIND] Variable " ++ printTree v ++ " not bound to expression."
    ERException name message -> "[EXCEPTION] Exception thrown. " ++ name ++ ": " ++ message
    ERInternal code message -> "[EINTERNAL] This should not happen (" ++ show code ++ "). " ++ message 
    ERNoMatching e -> "[ENOMATCHING] No expression matches value: " ++ printTree e
    ERParseFailed verbose path ts msg -> 
      "[EPARSEFAILED] Parse Failed... " ++ show path ++ "\n" 
      ++ (if verbose then "Tokens: " ++ show ts ++ "\n" else "") 
      ++ msg
    ERUnknown s -> "[EUNKNOWN] " ++ s
    EROperator op es -> "[EOPERATOR] Operator " ++ op ++ " can not be applied to the following arguments: " ++ show es 
    ERMatchingFailed -> "[EMATCHINGFAILED] User should never see this error..."
    ERPushMatching i s -> "[EMATCHING] case " ++ show i ++ " " ++ show s 
    ERStackTrace es -> intercalate "\n" (map show . reverse $ es)

instance Exception EError

-- Miscellaneous

bad :: String -> AResult a
bad = throwError . ERUnknown

badInternal :: Int -> String -> AResult a
badInternal i = throwError . ERInternal i

-- e can be SomeException or UFailure ...
appendSomeTrace :: Show e => EError -> e -> AResult a
appendSomeTrace e' e = throwError $ ERStackTrace [e', ERUnknown (show e)]

appendTrace :: EError -> EError -> AResult a
appendTrace e' e = case e of
  ERStackTrace es -> throwError $ ERStackTrace (e':es)
  _ -> throwError $ ERStackTrace [e',e]

throwBad :: String -> ScopeM -> ScopeM
throwBad name env = case env of
  Bad _ -> env
  Ok _ -> Bad name

throwBadMatching :: Int -> String -> ScopeM -> ScopeM
throwBadMatching i s = throwBad . show $ ERPushMatching i s

askE:: AResult Scope
askE = do
  menv <- ask
  case menv of
    Ok env -> return env
    Bad s -> bad s

failure :: Show a => a -> Result
failure x = fail $ "Undefined case: " ++ show x

unimplemented :: String -> Result
unimplemented = fail

-- TODO AESON
traceEnvironment :: Result
traceEnvironment = do
  context <- get
  scope <- askE
  pushToOstream "GlobalMemory"
  forM_ (Map.toList $ context ^. hglobals . memory) $ \(k, v) ->
    pushToOstream $ "  " ++ show k ++ " -> " ++ show v
  pushToOstream "LocalMemory"
  forM_ (Map.toList $ scope ^. hlocals) $ \(k, v) ->
    pushToOstream $ printTree k ++ " -> " ++ show v

traceInput :: Statement -> Result
traceInput x =
  pushToOstream $ "(ocaml) " ++ printTree x

traceExecutionStatistics :: Result
traceExecutionStatistics = do
  context <- get
  pushToOstreamFlagged FTraceExecutionStatistics 
    $  "Execution statistics:"
    ++ "\n  Allocated pointers: " ++ show ((context ^. hglobals . nextptr) - 1) -- Root is not included.
    ++ "\n  Used fresh variables: " ++ show (context ^. htypes . nextfresh)

-- XDD
traceTrace :: Show e => e -> Result
traceTrace e = do
  pushToOstream "Stack trace:"
  pushToOstream $ show e

-- Memory instances

instance MemoryMonad AResult Ptr Ident Expression where
  memMalloc = do
    context <- get
    let p = context ^. hglobals . nextptr
    memIncRef p
    modify $ over (hglobals . nextptr) (+1)
    return p

  memRead p = do
    context <- get
    return $ Map.lookup p $ context ^. hglobals . memory

  memWrite p v = case v of
    ERecord lstmts ->
      forM_ lstmts $ \lstmt -> do
        let VSimple (VVariable [ident]) = lstmt ^. lensVariableFromLabeledStatement
        let e = lstmt ^. lensExpressionFromLabeledStatement
        ptr <- memMakeDirectory p [ident]
        memWrite ptr e
    _ -> modify $ over (hglobals . memory) (Map.insert p v)

  memFree p = do
    c <- memRefCount p
    memDecRef p
    when (c == 1) $ do
      modify $ over (hglobals . memory) (Map.delete p)
      modify $ over (hglobals . counters) (Map.delete p)

  memRefCount p = do
    context <- get 
    return . fromMaybe 0 . Map.lookup p $ context ^. hglobals . counters

  memGetUsed e = case e of
    ERecordMemory pexprs -> return $ map snd pexprs
    _ -> return []

  memIncRef p = 
    modify $ over (hglobals . counters) (Map.alter maybeIncrement p)

  memDecRef p = 
    modify $ over (hglobals . counters) (Map.alter maybeDecrement p)

  memClean = error "HEHE"
    -- context <- get
    -- memRoot >>= memFreeRecursive
    -- let cbindings = Map.toList $ context ^. hglobals . namespace
    -- pushToOstream $ "(memClean ) " ++ show cbindings
    -- forM_ cbindings $ memFreeRecursive . snd
    -- modify $ over (hglobals . namespace . bindings) (const mempty)
    -- return ()

  memCopyRecord p = do
    v <- memRead p
    case v of
      Just e -> case e of
        ERecordMemory fields -> 
          return $ EReferenceMemory p
          -- One level copy ...
          -- fields' <- forM fields $ \(i, p) -> do
          --   e' <- memCopyRecord p
          --   return $ SLabeled (VSimple $ VVariable [i]) e'
          -- return $ ETypeableExpression $ ERecord $ fields'
        _ -> return e
      Nothing -> bad $ "(memCopyRecord) " ++ show p

  memOverwrite p e = do
    old_e <- memRead p
    case (old_e, e) of
      (Just old_expr, expr) ->
        case (old_expr, expr) of
          (ERecordMemory old_fields, ERecord fields) -> do
            forM_ old_fields $ memFree . snd -- TODO?
            memWrite p expr
          _ -> memWrite p expr
      _ -> undefined 

  memRoot = do
    context <- get
    return $ context ^. hglobals . rootptr
  
  memChangeDirectory p xs = case xs of
    [] -> return . Just $ p
    h:tl -> do
      x <- memRead p
      case x of
        Just e -> case e of
          EReferenceMemory p' -> memChangeDirectory p' xs
          ERecordMemory exprs -> case filter (\x -> fst x == h) exprs of
            [(i, ptr)] -> memChangeDirectory ptr tl
            _ -> return Nothing 
        _ -> return Nothing

  memMakeDirectory p xs = case xs of
    [] -> return p
    h:tl -> do
      v <- memRead p 
      case v of
        Just e -> case e of
          ERecordMemory exprs -> case filter (\x -> fst x == h) exprs of
            [(i, ptr)] -> memMakeDirectory ptr tl
            _ -> appendPath p xs exprs
          _ -> bad $ "(memMakeDirectory) Cannot create directory " ++ printTree xs ++ " in " ++ printTree e
        Nothing -> appendPath p xs []
      where
        appendPath :: Ptr -> NName -> [(Ident, Ptr)] -> AResult Ptr
        appendPath p (h:tl) exprs = do
          ptr <- memMalloc
          memIncRef ptr
          memWrite p $ ERecordMemory $ (h, ptr):exprs
          memMakeDirectory ptr tl

-- Context modifiers (Simple)

isConstructor :: NName -> AResult Bool
isConstructor x = case x of
  [name] -> do
    context <- get
    return $ Map.member name (context^.htypes.typedconstructors)
  _ -> return False

peekFromEnvironment :: CanonicalName -> AResult Expression
peekFromEnvironment variable = do
  lstmt <- peekVariable variable
  case lstmt of
    Just x -> return x
    Nothing -> do
      traceEnvironment
      throwError . ERBind . VSimple . VCanonical $ variable

checkScopedVariable :: NName -> AResult (Maybe Ptr)
checkScopedVariable path = do 
  scope <- askE
  return $ Map.lookup path (scope ^. hlocals)

peekLocation :: CanonicalName -> AResult (Maybe Ptr)
peekLocation name = do
  let path = fvPath name
  p <- checkScopedVariable (take 3 path)
  case p of
    Just p -> memChangeDirectory p (drop 3 path)
    Nothing -> do
      proot <- memRoot
      memChangeDirectory proot path

peekVariable :: CanonicalName -> AResult (Maybe Expression)
peekVariable name = do 
  p <- peekLocation name
  case p of
    Just ptr -> do
      res <- memCopyRecord ptr
      return $ Just res
    Nothing -> return Nothing

pushAssign :: CanonicalName -> Expression -> Result
pushAssign name e = do
  p <- peekLocation name
  case p of
    Nothing -> undefined
    Just ptr -> memOverwrite ptr e

withLocal :: LabeledStatement -> AResult a -> AResult a
withLocal lstmt stmt = do
  let v@(VSimple (VCanonical name)) = lstmt ^. lensVariableFromLabeledStatement
  let e = lstmt ^. lensExpressionFromLabeledStatement

  let (vs, ve) = splitAt 3 (fvPath name)
  withSharedPtr $ \p -> do
    p' <- memMakeDirectory p ve
    memWrite p' e 
    local (\env -> over hlocals (Map.insert vs p) <$> env) stmt 

withLocals :: [LabeledStatement] -> AResult a -> AResult a
withLocals lstmts stmt = case lstmts of
  [] -> stmt
  h:tl -> withLocal h (withLocals tl stmt)

pushToEnvironment :: LabeledStatement -> Result
pushToEnvironment lstmt = do
  let variable = lstmt ^. lensVariableFromLabeledStatement
  let expression = lstmt ^. lensExpressionFromLabeledStatement
  case variable of
    VSimple (VCanonical name) -> do
      proot <- memRoot
      p <- memMakeDirectory proot (fvPath name)
      memWrite p expression
    _ -> error $ "Not a canonical name: " ++ show variable

pushScopedVariableVersion :: CanonicalIndex -> Int -> ScopeM -> ScopeM
pushScopedVariableVersion index version env = 
  over localversionmanager (Map.insert index version) <$> env

pushToOstream :: String -> Result
pushToOstream = liftIO . putStrLn

pushToOstreamFlagged :: Flag -> String -> Result
pushToOstreamFlagged flag s = do
  context <- get
  when (Set.member flag $ context ^.hflags) $ pushToOstream s

pushConstructor :: Ident -> Ident -> Result
pushConstructor constructor typedef =
  modify $ over (htypes . typedconstructors) (Map.alter (maybeFailTypes typedef) constructor)
  where
    maybeFailTypes :: (Print a) => a -> Maybe a -> Maybe a
    maybeFailTypes v l = case l of
      Nothing -> Just v
      Just x -> error $ "Overlapping types : " ++ printTree x ++ " and " ++ printTree v

pushConstraint :: DebugWrapper TypeConstraint -> Result
pushConstraint t =
  modify $ over (htypes . constraints) (t:)

popConstraints :: Result
popConstraints = do
  modify $ over (htypes . constraints) (const [])
  modify $ over (htypes . variables) (Bimap.filter (\k v -> not . isLocalVariable $ k))
