--
-- This file is responsible 'mainly' for argument parsing.
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoCPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.Except (catchError, runExceptT)
import Control.Monad.Reader (liftIO, local, runReaderT)
import Control.Monad.Trans.State (runStateT)

import Data.Aeson (Value(..), (.=), object)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as Set
import Data.Version (showVersion)
import Exon (exon)
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)

import Context hiding (Parser)
import Flags
import Grammar
import Interpreter
import Operators ()
import Paths_ocamlhi (version)

data Options = Options
  { includes :: [String]
  , nodefaultflags :: Bool
  , flags :: [Flag]
  , file :: Maybe String
  } deriving (Show)

instance Iso Options Value where
  to x =
    object
      [ "includes" .= includes x
      , "flags" .= map show (Main.flags x)
      , "file" .= file x
      , "no-default-flags" .= nodefaultflags x
      ]
  from = undefined

-- Flags
flagOption :: Parser Flag
flagOption =
  foldl1 (<|>) (map toOption $ Set.toList $ Set.delete FUseLogBuffer allFlags)
  where
    toOption :: Flag -> Parser Flag
    toOption f = flag' f (long (show f) <> help (flagDescription f))

fileOption :: Parser String
fileOption = argument str (metavar "FILE" <> help "File to interpret.")

includeOption :: Parser String
includeOption =
  strOption
    (short 'I'
       <> long "include"
       <> metavar "INCLUDE_DIR"
       <> help
            "Standard library and other files will be loaded from this directory.")

nodefaultflagsOption :: Parser Bool
nodefaultflagsOption =
  switch
    (short 'f' <> long "no-default-flags" <> help "Do not use default flags.")

versionOption :: Parser (a -> a)
versionOption =
  infoOption (showVersion version) (long "version" <> help "Show version.")

optionsParser :: ParserInfo Options
optionsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> header "ocamlhi - Caml interpreter written in Haskell"
      -- <> progDesc "" 
      -- <> footer ""
     )

programOptions :: Parser Options
programOptions =
  Options
    <$> many includeOption
    <*> nodefaultflagsOption
    <*> many flagOption
    <*> optional fileOption

read :: String -> InterpreterT Toplevel
read code = do
  context <- getContext
  let lexer' = context ^. hfiles . lexer
  let parser' = context ^. hfiles . parser
  case parser' . lexer' $ code of
    Left msg -> bad Nothing $ show msg
    Right tree -> return tree

interpretFile :: FilePath -> InterpreterT ()
interpretFile path = do
  _ <-
    local
      (setCurrentFile "toplevel"
         . resetFlag FTraceInput
         . resetFlag FTraceOutput)
      (Main.read [exon|#use "stdlib.ml"|] >>= prettyEval)
  _ <-
    local
      (setCurrentFile "toplevel")
      (Main.read [exon|#use "#{path}"|] >>= prettyEval)
  traceExecutionStatistics
  `catchError` (\e -> do
                  Context.log $ LogError e
                  liftIO exitFailure)

main :: IO ()
main = do
  opts <- execParser optionsParser
  flags' <-
    if nodefaultflags opts
      then return . Set.fromList . Main.flags $ opts
      else initial
  print
    $ if Set.member FTraceHello flags'
        then LogHelloImage
        else LogHello
  B.putStrLn . encodePretty
    $ (to $ opts {Main.flags = Set.toList flags'} :: Value)
  scope <- initial
  context <- initial
  let filehandler = context ^. hfiles
  let filehandler' = filehandler {_includeddirectories = includes opts}
  let context' = context {_hfiles = filehandler'}
  let scope' = scope {Context._flags = flags'}
  case file opts of
    Nothing -> do
      putStrLn "Interactive mode is not implemented."
      exitFailure
    Just path -> do
      _ <-
        runReaderT
          (runStateT (runExceptT (interpretFile path)) context')
          (Right scope')
      exitSuccess
