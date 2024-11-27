module TopologicalSort (
  topologicalSort
  ) where

import Data.Array (array)
import qualified Data.Bimap as Bimap
import Data.Graph (topSort)
import Data.List (nub, partition, (\\), elemIndex, intersect)

{-# ANN module "HLint: ignore Use camelCase" #-}

topologicalSort :: (Eq a, Ord a, Show a) => [(a, [a])] -> [a]
topologicalSort e =
  let s = nub . concatMap (uncurry (:)) $ e
      n = length s
      m = Bimap.fromList $ zip s [0..n - 1]
      e' = map (\(v, es) -> let f x = m Bimap.! x in (f v, map f es)) e
      graph = array (0, n-1) e'
      res = reverse . topSort $ graph
  in case partition (< 0) res of
    ([], xs) -> map (m Bimap.!>) res
    _ -> []
