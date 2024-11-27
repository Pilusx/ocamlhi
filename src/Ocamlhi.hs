--
-- This file is responsible 'mainly' for argument parsing.
--
module Main where

import Control.Monad (forM_)
import Control.Monad.Except (catchError, runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.State (execStateT, runStateT)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Context
import Dependencies
import ErrM
import Grammar
import Initializable
import Interpreter
import PrintOcaml
import TopologicalSort (topologicalSort)

-- Main function
class (Show a, Print a) => Entrypoint a where
  enter :: a -> Result
  enter x = failure $ "This type is not an entrypoint" ++ show x
  
  interpret :: a -> IO ()
  interpret x = error $ "This type is not an entrypoint" ++ show x

  showTree :: Verbosity -> a -> IO ()
  showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

  run :: Verbosity -> a -> IO ()
  run v tree = do 
    putStrLn "\nParse Successful!"
    showTree v tree
    putStrLn "\n[Interpretation]\n"
    interpret tree
    exitSuccess

instance Entrypoint Prog where
  enter x = do { 
    translate x; 
    pushToOstream "Interpreter Successful\n";
    traceExecutionStatistics
    } `catchError` traceTrace

  interpret tree = do
    let prog = enter tree
    let context = initial :: Context
    let scope = Ok (initial :: Scope)
    runExceptT $ runReaderT (runStateT prog context) scope
    return ()

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = do
  let context = initial {_dsverbosity=v} :: DependenciesState
  res <- runExceptT $ execStateT (findDependencies f) context
  case res of
    Left error -> do
      print error
      exitFailure
    Right (DependenciesState g _ _ _) -> do
      let order = topologicalSort $ getDependencies g
      let source = getSource g order 
      putStrLn f
      putStrLn "[Dependencies]\n" 
      forM_ order putStrLn
      run v source

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely." -- [TODO] It is probably broken now..
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitSuccess

{-# ANN main "HLint: ignore Use getContents" #-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    "-s":fs -> mapM_ (runFile 0) fs
    fs -> mapM_ (runFile 2) fs

