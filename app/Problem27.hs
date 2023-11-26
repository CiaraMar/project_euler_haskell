module Problem27 where

import Data.List (maximumBy)
import Data.Numbers.Primes (primes)
import Data.Ord (comparing)
import qualified Data.Set as Set (fromDistinctAscList, member)

problem27 :: Int
problem27 = fst $ maximumBy (comparing snd) [(a * b, chainLength a b) | a <- [-999 .. 999], b <- [-1000 .. 1000]]
  where
    pset = Set.fromDistinctAscList . takeWhile (<= 3000000) $ primes
    quadratic a b = map (\n -> n * n + a * n + b) [0 ..]
    chainLength a b = length . takeWhile (`Set.member` pset) $ quadratic a b
