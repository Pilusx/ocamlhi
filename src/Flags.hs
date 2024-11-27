--
-- This file contains user-configurable flags.
-- They can be enabled using the #enable <flag> statement.
--

module Flags where

import Data.Bimap
import qualified Data.Set as Set

data Flag 
  = FTraceInputTree 
  | FTraceInputVersion 
  | FTraceOutputTree
  | FTraceTypeConstraints
  | FTraceFunctionCalls
  | FTraceExecutionStatistics
  deriving (Eq, Ord)

availableFlags :: Bimap String Flag
availableFlags = fromList [
  ("trace-input-tree", FTraceInputTree),
  ("trace-input-version", FTraceInputVersion),
  ("trace-output-tree", FTraceOutputTree),
  ("trace-type-constraints", FTraceTypeConstraints),
  ("trace-function-calls", FTraceFunctionCalls),
  ("trace-execution-statistics", FTraceExecutionStatistics)
  ]

allFlags :: [Flag]
allFlags = keysR availableFlags

defaultFlags :: Set.Set Flag
defaultFlags = Set.fromList [FTraceExecutionStatistics]

instance Show Flag where
  show f = availableFlags !> f

readFlag :: String -> Flag
readFlag s = availableFlags ! s
