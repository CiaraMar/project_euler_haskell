module Problem14 where

import Data.Array ((!), assocs, listArray)
import Data.Foldable (maximumBy)
import Data.Function (on)

collatzArray :: Int -> [(Int, Int)]
collatzArray n = assocs arr
  where
    arr = listArray (1, n) $ 0 : map collatz [2 ..]
    collatz i = collatzCheck (collatzStep i) + 1
    collatzCheck i =
      if i <= n
        then arr ! i
        else collatz i
    collatzStep i =
      if even i
        then i `div` 2
        else 3 * i + 1

problem14 :: Int
problem14 = fst . maximumBy (compare `on` snd) . collatzArray $ 1000000
