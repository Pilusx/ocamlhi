--
-- Implementation of the dependency resolution algorithm.
-- It is used for finding cycles and the topological order of #use directives.
--
{-# LANGUAGE TemplateHaskell #-}
module Dependencies where

import Control.Lens ((^.), makeLenses, over)
import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, modify)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import System.FilePath((</>), takeDirectory)

import Context (EError(..))
import ErrM
import Grammar
import LexOcaml (Token)

-- Printing utils
type Verbosity = Int

-- [TODO] Use Distribution.Verbosity ?
isVerbose :: Verbosity -> Bool
isVerbose v = v > 1

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (isVerbose v) $ putStrLn s

-- Structures
type Lexer = String -> [Token]
type Parser a = [Token] -> Err a

data DependencyGraph = DependencyGraph {
  _gdependencies :: Map.Map FilePath [FilePath], 
  _gsource :: Map.Map FilePath Prog
}

data DependenciesState = DependenciesState {
  _dsdependencygraph :: DependencyGraph,
  _dsverbosity :: Verbosity,
  _dsparser :: Parser Prog,
  _dslexer :: Lexer
}

$(makeLenses ''DependencyGraph)
$(makeLenses ''DependenciesState)

type DependenciesT = StateT DependenciesState (ExceptT EError IO)

-- TODO try to force the inline foldr implementation of mapMaybe
findIncludes :: Prog -> [String] 
findIncludes x = case x of
  Prog1 statements -> mapMaybe extractDependency statements where
    extractDependency s = case s of
      SDirective (NFile filename) -> Just filename 
      _ -> Nothing

getDependencies :: DependencyGraph -> [(FilePath, [FilePath])]
getDependencies = Map.toList . _gdependencies

getSource :: DependencyGraph -> [FilePath] -> Prog
getSource g = Prog1 . concatMap ((\(Prog1 xs) -> xs) . (Map.!) (g ^. gsource))

findDependencies :: FilePath -> DependenciesT ()
findDependencies path = do
  DependenciesState g v parse lex <- get
  unless (Map.member path $ g ^. gdependencies) $ do
    s <- lift . lift $ readFile path
    let ts = lex s 
    case parse ts of
      Bad msg -> lift . throwError $ ERParseFailed (isVerbose v) path ts msg
      Ok tree -> do
        let dependencies = map (takeDirectory path </>) $ findIncludes tree
        modify $ over (dsdependencygraph . gdependencies) $ Map.insert path $ nub dependencies
        modify $ over (dsdependencygraph . gsource) $ Map.insert path tree
        forM_ dependencies findDependencies
