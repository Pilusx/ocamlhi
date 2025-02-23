--
-- This file contains the main represenation of the interpreter's state (Context).
-- It provides API for state management and error handling.
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoCPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Context where

import Control.Exception (Exception)
import Control.Lens ((^.), makeLenses, over)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Extra (firstJustM)
import Control.Monad.Reader (ReaderT, ask, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, modify)
import Control.Unification (UTerm(..))
import Control.Unification.IntVar (IntVar(..))

import Data.Aeson (Key, Value(..), (.=), object)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Key (fromString)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Functor ((<&>))
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Version (showVersion)
import Exon (exon)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import ErrM
import Flags
import Grammar
import Lexer
import Parser
import Paths_ocamlhi (version)
import Print

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data LogLine
  = LogString String
  | LogHello
  | LogHelloImage
  | LogInput Statement
  | LogInputTree Statement
  | LogInputVersion Statement
  | LogTypeConstraint String (OpenTerm, Expr) (OpenTerm, Expr)
  | LogDefinition Expr Expr Expr
  | LogOutput Expr Expr
  | LogOutputTree Expr
  | LogDumpMemory String String Scope (Map.Map IntVar OpenTerm)
  | LogExecutionStatistics Int
  | LogError EError
  | LogWarning String

data EError
  = ERInternal String
  | ERException Expr
  | ERNoMatching Expr [Expr]
  | ERParseFailed FilePath String
  | ERUnknown BNFC'Position String
  | ERStackTrace [EError]

type Lexer = String -> [Token]

type Parser a = [Token] -> Err a

type LogBuffer = [LogLine]

data TypeHandler = TypeHandler
  { _types :: Storage Statement
  , _mutablefields :: Set.Set LowercaseIdent
  -- Unification
  , _nextfresh :: Int
  , _assignments :: Map.Map IntVar OpenTerm
  , _modulesignatures :: Map.Map Path [Statement]
  -- Constants
  , _tinteger :: IntVar
  , _tfloat :: IntVar
  , _tchar :: IntVar
  , _tstring :: IntVar
  , _tbool :: IntVar
  , _tlist :: IntVar
  , _tunit :: IntVar
  , _texn :: IntVar
  , _tany :: IntVar
  }

data FileHandler = FileHandler
  { _includeddirectories :: [FilePath]
  , _lexer :: Lexer
  , _parser :: Parser Toplevel
  }

data Context = Context
  { _hmemory :: Memory
  , _hversions :: Storage (Integer, Bool)
  , _htypes :: TypeHandler
  , _hfiles :: FileHandler
  , _hlogbuffer :: LogBuffer
  }

data Scope = Scope
  { _visiblelocal :: Map.Map OIdent Integer
  , _localmemory :: Map.Map (OIdent, Integer) Memory
  , _isrecursive :: Set.Set Path
  , _flags :: Set.Set Flag
  , _currentfile :: FilePath
  , _modulename :: [UppercaseIdent]
  , _hasTag :: Tag
  , _hasNameContext :: NameContext
  }

$(makeLenses ''FileHandler)

$(makeLenses ''TypeHandler)

$(makeLenses ''Context)

$(makeLenses ''Scope)

type ScopeM = Err Scope

type InterpreterT_ = StateT Context (ReaderT ScopeM IO)

type InterpreterT = ExceptT EError InterpreterT_

-- Scoped variables
resetFlag :: Flag -> ScopeM -> ScopeM
resetFlag flag scope = over flags (Set.delete flag) <$> scope

setCurrentFile :: FilePath -> ScopeM -> ScopeM
setCurrentFile path scope = over currentfile (const path) <$> scope

setFlag :: Flag -> ScopeM -> ScopeM
setFlag flag scope = over flags (Set.insert flag) <$> scope

setModuleName :: OIdent -> ScopeM -> ScopeM
setModuleName (IUppercase _ ident) scope = over modulename (ident :) <$> scope
setModuleName x _ = Left $ "setModuleName: Invalid module name " ++ printTree x

setNameContext :: NameContext -> ScopeM -> ScopeM
setNameContext nctx scope = over hasNameContext (const nctx) <$> scope

isRecursive :: Path -> Scope -> Bool
isRecursive path scope = Set.member path (scope ^. isrecursive)

setRecursive :: RecOptional -> Path -> ScopeM -> ScopeM
setRecursive (RecNo {}) _ scope = scope
setRecursive (RecYes {}) path scope =
  over isrecursive (Set.insert path) <$> scope

setTag :: Tag -> ScopeM -> ScopeM
setTag tag scope = over hasTag (const tag) <$> scope

setVisibleLocal :: CanonicalName -> ScopeM -> ScopeM
setVisibleLocal (CanonicalName _ [i] name v []) scope
  | i == identLocal =
    over visiblelocal (Map.insert (stripPosition name) v) <$> scope
setVisibleLocal name _ =
  fail $ "Cannot set " ++ show name ++ " as visible local"

setVisibleLocals :: [CanonicalName] -> ScopeM -> ScopeM
setVisibleLocals es = compose (map setVisibleLocal es)

resetModuleName :: ScopeM -> ScopeM
resetModuleName scope = over modulename (const []) <$> scope

-- Error handling & logging
shorten :: Expr -> IO Expr
shorten x =
  case x of
    Reference ref -> fromIODeep ref >>= shorten
    Record _ fields ->
      Record npos
        <$> mapM
              (\case
                 EField _ v e -> EField npos v <$> shorten e
                 _ -> undefined)
              fields
    ELambda {} -> return $ EConstant npos $ CFun npos
    EPartial {} -> return $ EConstant npos $ CFun npos
    EConstructor _ v e -> EConstructor npos v <$> shorten e
    _ -> return x

instance Show LogLine where
  show =
    \case
      LogString s -> s
      LogHello ->
        let v = showVersion version
         in "ocamlhi version " ++ v
      LogHelloImage ->
        let v = showVersion version
            t = v ++ replicate (11 - length v) ' '
         in [exon|                 :@@@@@@@@@@@@@@                            
            @@@@@:              @@@@%                       
         @@@   ocamlhi  #{    t   } @@=  @@@@:  @           
       @@                             @@@.=.::=*::@         
    -@@      @@@      @@@%@@@=         @ .@*::::=@@         
  @@.       @ %     @@.:::::.-@        @.::::.@@            
#@           :@@@@@:.::::.:::: @%      @ ::: @              
            @@ .:::::::.:.:::::.@@     @ ::.@    @@         
            @ :::::..:::::::::::. @@@@@.:::.@     @@        
            @ :::: @ :.:::::::.::::::::::: @       @@       
           @@:::: @@.:::::::::.::::::::::.@        :@:      
         @@.::=+@@ @=:::..:.:::::::::::.@=          @@      
         @:.@  *@..@ :@@@@@@. @@  @@@@=              @@     
         @: %@  @ .@       #.% @  @                  @@     
          @@@@  @@%#@@    @#.@ @. :@#                @@.    |]
      LogInput (Expression _ e) -> "(ocaml) " ++ printTree e
      LogInput stmt -> "(ocaml) " ++ printTree stmt
      LogInputTree stmt -> " input: " ++ show stmt
      LogInputVersion stmt -> "   ver: " ++ show stmt
      LogTypeConstraint msg (t1, t1') (t2, t2') ->
        intercalate
          "\n"
          [ "[Constraint] " ++ msg
          , "  " ++ show t1 ++ " as " ++ printTree t1'
          , "  " ++ show t2 ++ " as " ++ printTree t2'
          ]
      LogDefinition name type_ expression ->
        "   val: "
          ++ printTree name
          ++ " : "
          ++ printTree type_
          ++ " = "
          ++ printTree (unsafePerformIO $ shorten expression)
      LogOutput type_ expression ->
        "    - : "
          ++ printTree type_
          ++ " = "
          ++ printTree (unsafePerformIO $ shorten expression)
      LogOutputTree expression -> "output: " ++ show expression
      LogDumpMemory memory versions scope terms ->
        intercalate "\n"
          $ [ "Memory (global):"
            , memory
            , "Versions: "
            , versions
            , "Memory (local):"
            ]
              ++ map show (Map.toList $ scope ^. localmemory)
              ++ ["Visible locals:"]
              ++ map show (Map.toList $ scope ^. visiblelocal)
              ++ ["Terms:", "{"]
              ++ map (showString "  " . show) (Map.toList terms)
              ++ ["}"]
      LogExecutionStatistics freevars ->
        intercalate
          "\n"
          ["Execution statistics:", "Used fresh variables: " ++ show freevars]
      LogError err -> intercalate "\n" ["Stack trace:", show err]
      LogWarning msg -> "[WARN] " ++ show msg

instance Show EError where
  show =
    \case
      ERInternal msg -> "[EINTERNAL] Got exception. " ++ msg
      ERException value -> "[EXCEPTION] Exception thrown. " ++ printTree value
      ERNoMatching e patterns ->
        intercalate "\n"
          $ ("[ENOMATCHING] No expression matches value: " ++ printTree e)
              : map
                  (\x -> printTree (hasPosition x) ++ " " ++ printTree x)
                  patterns
      ERParseFailed path msg ->
        "[EPARSEFAILED] Parse Failed... " ++ show path ++ "\n" ++ msg
      ERUnknown pos@(Just {}) s -> "[EUNKNOWN] " ++ printTree pos ++ " " ++ s
      ERUnknown Nothing s -> "[EUNKNOWN] " ++ s
      ERStackTrace es -> intercalate "\n" (map show . reverse $ es)

instance Exception EError

-- Miscellaneous
matchFilePath :: FilePath -> InterpreterT (Maybe FilePath)
matchFilePath path = do
  context <- getContext
  firstJustM
    (\x -> do
       let path' = x </> path
       exists <- liftIO $ doesFileExist path'
       return
         $ if exists
             then Just path'
             else Nothing)
    (context ^. hfiles . includeddirectories)

log :: LogLine -> InterpreterT ()
log x = do
  scope <- getScope
  let flags' = scope ^. flags
  case x of
    LogExecutionStatistics {}
      | not (Set.member FTraceExecutionStatistics flags') -> return ()
    LogDefinition {}
      | not (Set.member FTraceOutput flags') -> return ()
    LogInput {}
      | not (Set.member FTraceInput flags') -> return ()
    LogInputTree {}
      | not (Set.member FTraceInputTree flags') -> return ()
    LogInputVersion {}
      | not (Set.member FTraceInputVersion flags') -> return ()
    LogOutput {}
      | not (Set.member FTraceOutput flags') -> return ()
    LogOutputTree {}
      | not (Set.member FTraceOutput flags')
          || not (Set.member FTraceOutputTree flags') -> return ()
    _
      | Set.member FUseLogBuffer flags' -> lift $ modify $ over hlogbuffer (x :)
    _ -> liftIO . print $ x

bad :: BNFC'Position -> String -> InterpreterT a
bad pos s = throwError $ ERUnknown pos s

appendTrace :: EError -> EError -> InterpreterT a
appendTrace e' e =
  case e of
    ERStackTrace es -> throwError $ ERStackTrace (e' : es)
    ERException {} -> throwError e
    _ -> throwError $ ERStackTrace [e', e]

getContext :: InterpreterT Context
getContext = lift get

getPosition :: BNFC'Position -> InterpreterT BNFC'Position
getPosition Nothing = return Nothing
getPosition (Just (_, line, col)) = do
  scope <- getScope
  return $ Just (scope ^. currentfile, line, col)

getMemory :: Expr -> InterpreterT (Maybe Memory)
getMemory x@(VCanonical _ name) = do
  context <- getContext
  case name of
    CanonicalName _ [i] n v ridents
      | i == identLocal -> do
        scope <- getScope
        case Map.lookup (n, v) (scope ^. localmemory) of
          Nothing -> bad Nothing $ "Cannot find local variable " ++ printTree x
          Just e -> go False (map (ILowercase npos) ridents) e
    CanonicalName {} -> go False (to name :: Path) (context ^. hmemory)
    _ -> undefined
getMemory _ = return Nothing

getModuleName :: InterpreterT [UppercaseIdent]
getModuleName = do
  scope <- getScope
  return $ reverse $ scope ^. modulename

getScope :: InterpreterT Scope
getScope = do
  menv <- ask
  case menv of
    Left s -> bad Nothing s
    Right env -> return env

getVersion :: Path -> InterpreterT (Integer, Bool)
getVersion path = do
  context <- getContext
  fs <- Context.readFile path (context ^. hversions)
  case fs of
    Nothing -> return (-1, True)
    Just x -> return x

makeModule :: BNFC'Position -> InterpreterT ()
makeModule pos = do
  path <- getModuleName
  context <- getContext
  let path' = map (IUppercase npos) path
  res <- go True path' (context ^. hmemory)
  _ <-
    case res of
      Nothing -> go False path' (context ^. hmemory)
      Just _ -> bad pos $ "Module " ++ printTree path' ++ " already exists."
  return ()

termOfMemory :: Memory -> InterpreterT IntVar
termOfMemory fs = do
  fs' <- liftIO $ readIORef fs
  return
    $ case fs' of
        File _ t -> t
        Directory _ t -> t

traceEnvironment :: InterpreterT ()
traceEnvironment = do
  context <- getContext
  scope <- getScope
  mem <- showR (context ^. hmemory)
  ver <- showR (context ^. hversions)
  let terms = context ^. htypes . assignments
  Context.log $ LogDumpMemory mem ver scope terms

traceExecutionStatistics :: InterpreterT ()
traceExecutionStatistics = do
  context <- getContext
  Context.log $ LogExecutionStatistics (context ^. htypes . nextfresh)

-- Utils
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

-- Constants
typeBool :: Path
typeBool = [ILowercase npos . LowercaseIdent $ "bool"]

typeChar :: Path
typeChar = [ILowercase npos . LowercaseIdent $ "char"]

typeExn :: Path
typeExn = [ILowercase npos . LowercaseIdent $ "exn"]

typeFloat :: Path
typeFloat = [ILowercase npos . LowercaseIdent $ "float"]

typeInt :: Path
typeInt = [ILowercase npos . LowercaseIdent $ "int"]

typeList :: Path
typeList = [ILowercase npos . LowercaseIdent $ "list"]

typeString :: Path
typeString = [ILowercase npos . LowercaseIdent $ "string"]

typeUnit :: Path
typeUnit = [ILowercase npos . LowercaseIdent $ "unit"]

identContents :: LowercaseIdent
identContents = LowercaseIdent "contents"

identExn :: LowercaseIdent
identExn = LowercaseIdent "exn"

identList :: LowercaseIdent
identList = LowercaseIdent "list"

identLocal :: UppercaseIdent
identLocal = UppercaseIdent ".local"

temporaryVariable :: Expr
temporaryVariable =
  case toParameters 1 of
    [temp] -> temp
    _ -> undefined

unit :: Expr
unit = EConstant npos $ CUnit1 npos

termList :: OpenTerm -> Term OpenTerm
termList x = TermPolymorphic x (UTerm $ TermVariable typeList)

-- Properties
isTrue :: Expr -> Bool
isTrue =
  \case
    EConstant _ (CTrue _) -> True
    _ -> False

isLowercase :: OIdent -> Bool
isLowercase =
  \case
    ILowercase {} -> True
    _ -> False

isUppercase :: OIdent -> Bool
isUppercase =
  \case
    IUppercase {} -> True
    _ -> False

isLocalVariable :: CanonicalName -> Bool
isLocalVariable n =
  case n of
    CanonicalName {moduleidents = m:_} -> m == identLocal
    _ -> False

-- Converters
toLambda :: [Expr] -> Expr -> Expr
toLambda params e =
  if null params
    then e
    else ELambda npos (Fun1 npos) params e

toParameters :: Int -> [Expr]
toParameters n =
  [ Variable npos [ILowercase npos $ LowercaseIdent ("." ++ show i)]
  | i <- [1 .. n]
  ]

-- Initialization
class Initializable a where
  initial :: IO a

instance Initializable () where
  initial = return ()

instance Initializable IntVar where
  initial = return . IntVar $ 0

instance Initializable (Integer, Bool) where
  initial = return (-1, False)

instance Initializable [a] where
  initial = return mempty

instance (Ord k, Initializable t) => Initializable (IORef (FileSystem k v t)) where
  initial = do
    t <- initial
    newIORef $ Directory mempty t

instance Initializable (Set.Set Flag) where
  initial = return $ Set.fromList [FTraceInput, FTraceOutput] -- allFlags Set.\\ Set.fromList [FUseLogBuffer]

instance Initializable Statement where
  initial = return (Expression npos . EConstant npos $ CUnit1 npos)

instance Initializable TypeHandler where
  initial = do
    types' <- initial
  -- Initialize exceptions
    let exn = ILowercase npos identExn
    let list = ILowercase npos identList
    let emptyTypedef name =
          Typedef npos (Variable npos [name]) (PatternPrefixNo npos) []
    writeIO exn (emptyTypedef exn) () types'
    writeIO list (emptyTypedef list) () types'
    return
      $ TypeHandler
          { _types = types'
          , _mutablefields = mempty
      -- Unification
          , _nextfresh = 10
          , _assignments =
              Map.fromList . map (bimap IntVar UTerm)
                $ [ (1, TermVariable typeInt)
                  , (2, TermVariable typeFloat)
                  , (3, TermVariable typeChar)
                  , (4, TermVariable typeString)
                  , (5, TermVariable typeBool)
                  , (6, termList . UVar . IntVar $ 9)
                  , (7, TermVariable typeUnit)
                  , (8, TermVariable typeExn)
                  ]
          , _modulesignatures = mempty
      -- Constants
          , _tinteger = IntVar 1
          , _tfloat = IntVar 2
          , _tchar = IntVar 3
          , _tstring = IntVar 4
          , _tbool = IntVar 5
          , _tlist = IntVar 6
          , _tunit = IntVar 7
          , _texn = IntVar 8
          , _tany = IntVar 9
          }

instance Initializable FileHandler where
  initial =
    return
      $ FileHandler
          {_includeddirectories = [], _lexer = myLexer, _parser = pToplevel}

instance Initializable Context where
  initial = do
    hmemory' <- initial
    hversions' <- initial
    htypes' <- initial
    hfiles' <- initial
    return
      $ Context
          { _hmemory = hmemory'
          , _hversions = hversions'
          , _htypes = htypes'
          , _hfiles = hfiles'
          , _hlogbuffer = mempty
          }

instance Initializable Scope where
  initial =
    return
      $ Scope
          { _visiblelocal = mempty
          , _localmemory = mempty
          , _isrecursive = mempty
          , _flags = mempty
          , _currentfile = mempty
          , _modulename = mempty
          , _hasTag = TagExpression
          , _hasNameContext = CtxUsage
          }

-- Filesystem utils
class IsoR a b where
  toR :: a -> InterpreterT b
  fromR :: b -> InterpreterT a

instance {-# OVERLAPPABLE #-} Initializable v => IsoR v (Storage v) where
  toR f =
    liftIO $ do
      f' <- File f <$> initial
      newIORef f'
  fromR fs =
    liftIO (readIORef fs)
      >>= (\case
             File v _ -> return v
             Directory _ _ -> liftIO initial)

instance IsoR Expr Memory where
  toR x =
    case x of
      Record _ fields -> do
        dir <-
          Directory . Map.fromList <$> fieldsToMemory fields <*> liftIO initial
        liftIO $ newIORef dir
      Reference fs -> return fs
      _ ->
        liftIO $ do
          t <- initial
          newIORef (File x t)
    where
      fieldsToMemory :: [Field] -> InterpreterT [(OIdent, Memory)]
      fieldsToMemory =
        mapM
          (\case
             EField _ (VCanonical _ (CanonicalField k)) v -> do
               v' <- toR v
               return (ILowercase npos k, v')
             _ -> undefined)
  fromR fs = do
    fs' <- liftIO $ readIORef fs
    case fs' of
      File v _ -> return v
      Directory {} -> return $ Reference fs

go ::
     (Ord k, Initializable t)
  => Bool
  -> [k]
  -> IORef (FileSystem k v t)
  -> InterpreterT (Maybe (IORef (FileSystem k v t)))
go readonly path fs =
  case path of
    [] -> return . Just $ fs
    p:ps -> do
      fs' <- liftIO $ readIORef fs
      case fs' of
        File {} -> return Nothing
        Directory m _ ->
          case Map.lookup p m of
            Just old_fs -> go readonly ps old_fs
            Nothing ->
              if readonly
                then return Nothing
                else do
                  t <- liftIO initial
                  new_fs <- liftIO $ newIORef (Directory mempty t)
                  liftIO
                    $ modifyIORef
                        fs
                        (\case
                           Directory m' t' ->
                             Directory (Map.insert p new_fs m') t'
                           _ -> undefined)
                  go readonly ps new_fs

mkdir ::
     (Ord k, Initializable t)
  => [k]
  -> IORef (FileSystem k v t)
  -> InterpreterT (Maybe (IORef (FileSystem k v t)))
mkdir = go False

read ::
     (Ord k, Initializable t, IsoR v (IORef (FileSystem k v t)))
  => [k]
  -> IORef (FileSystem k v t)
  -> InterpreterT (Maybe v)
read path fs = do
  fs' <- go True path fs
  case fs' of
    Nothing -> return Nothing
    Just fs'' -> Just <$> fromR fs''

readFile ::
     (Ord k, Initializable t)
  => [k]
  -> IORef (FileSystem k v t)
  -> InterpreterT (Maybe v)
readFile path fs = do
  fs' <- go True path fs
  case fs' of
    Just fs'' -> do
      fs''' <- liftIO (readIORef fs'')
      case fs''' of
        File v _ -> return . Just $ v
        _ -> return Nothing
    _ -> return Nothing

instance IsoR Key OIdent where
  toR = undefined
  fromR = return . fromString . show

instance Show a => IsoR Value a where
  toR = undefined
  fromR = return . String . T.pack . show

toObject ::
     (Show t, IsoR Key k, IsoR Value v)
  => IORef (FileSystem k v t)
  -> InterpreterT Value
toObject fs =
  liftIO (readIORef fs)
    >>= (\case
           File v t -> do
             let t' = String . T.pack . show $ t
             v' <- fromR v
             return
               $ object
                   [ fromString "metadata" .= t'
                   , fromString "value" .= (v' :: Value)
                   ]
           Directory entries t -> do
             object
               <$> if Map.null entries
                     then do
              -- It is probably a local variable...
                       let t' = String . T.pack . show $ t
                       return [fromString "metadata" .= t']
                     else mapM
                            (\(k, v) -> do
                               k' <- fromR k
                               v' <- toObject v
                               return $ k' .= v')
                            (Map.toList entries))

showR ::
     (Show t, IsoR Key k, IsoR Value v)
  => IORef (FileSystem k v t)
  -> InterpreterT String
showR fs = toObject fs <&> B.unpack . encodePretty

write ::
     ( Show k
     , Show t
     , Ord k
     , Initializable t
     , IsoR Key k
     , IsoR Value v
     , IsoR v (IORef (FileSystem k v t))
     )
  => [k]
  -> v
  -> IORef (FileSystem k v t)
  -> InterpreterT ()
write path v fs
  | not (null path) = do
    _ <- mkdir path fs
    fs' <- go True (init path) fs
    case fs' of
      Nothing -> bad Nothing $ "write " ++ show path
      Just fs'' -> do
        new_fs <- toR v
        liftIO
          $ modifyIORef
              fs''
              (\case
                 Directory m t -> Directory (Map.insert (last path) new_fs m) t
                 _ -> undefined)
write _ _ _ = undefined

writeIO :: (Ord k) => k -> v -> t -> IORef (FileSystem k v t) -> IO ()
writeIO k v t fs = do
  new_fs <- newIORef (File v t)
  modifyIORef
    fs
    (\case
       Directory m' t' -> Directory (Map.insert k new_fs m') t'
       _ -> undefined)
