--
-- This file contains user-configurable flags.
--
module Flags where

import qualified Data.Set as Set

import Grammar
import Operators ()

data Flag
  = FTraceExecutionStatistics
  | FTraceHello
  | FTraceInput
  | FTraceInputTree
  | FTraceInputVersion
  | FTraceTypeConstraints
  | FTraceOutput
  | FTraceOutputTree
  | FUseLogBuffer
  deriving (Eq, Ord)

instance Show Flag where
  show flag =
    case flag of
      FTraceExecutionStatistics -> "trace-execution-statistics"
      FTraceHello -> "trace-hello"
      FTraceInput -> "trace-input"
      FTraceInputTree -> "trace-input-tree"
      FTraceInputVersion -> "trace-input-version"
      FTraceTypeConstraints -> "trace-type-constraints"
      FTraceOutput -> "trace-output"
      FTraceOutputTree -> "trace-output-tree"
      FUseLogBuffer -> "use-log-buffer"

flagDescription :: Flag -> String
flagDescription flag =
  case flag of
    FTraceExecutionStatistics ->
      "Print execution statistics at the end of the program. "
        ++ "Statistics include allocated pointers and used 'fresh variables'."
    FTraceHello ->
      "Print the ASCII image before executing any actions. If not used a simplified message of version is displayed."
    FTraceInput ->
      "Print the input statements. Example: " ++ "(ocaml) let x = 3"
    FTraceInputTree ->
      "Print input trees after each global statement. Example: "
        ++ "input: "
        ++ show (Variable npos [ILowercase npos $ LowercaseIdent "x"] :: Expr)
    FTraceInputVersion ->
      "Print input tree with bound variables after each global statement. Example: "
        ++ "ver: "
        ++ show
             (VCanonical
                Nothing
                (CanonicalName
                   TagExpression
                   []
                   (ILowercase npos $ LowercaseIdent "x")
                   0
                   []) :: Expr)
    FTraceTypeConstraints ->
      "Print all constraints found during type resolution stage."
    FTraceOutput ->
      "Prints the result of definition or expression. When disabled it also disables a flag 'trace-output-tree'. Example: "
        ++ "   val: x\v.0 : int = 3"
    FTraceOutputTree ->
      "Print the tree of the result. Example: "
        ++ "output: "
        ++ show ((from :: Integer -> Expr) 3)
    FUseLogBuffer ->
      "Log all data to the internal buffer. All logs have to be manually extracted. "
        ++ "It is only working in the webserver for sending logs as json."

allFlags :: Set.Set Flag
allFlags =
  Set.fromList
    [ FTraceHello
    , FTraceInput
    , FTraceInputTree
    , FTraceInputVersion
    , FTraceTypeConstraints
    , FTraceOutput
    , FTraceOutputTree
    , FTraceExecutionStatistics
    , FUseLogBuffer
    ]
