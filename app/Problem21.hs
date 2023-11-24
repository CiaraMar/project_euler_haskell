module Problem21 where

import Data.Array ((!), assocs, listArray)

import Utils (properDivisors)

problem21 :: Int
problem21 = sum . map fst . filter (uncurry isAmicable) . assocs $ arr
  where
    arr = listArray (0, 10000) $ 0 : map (sum . properDivisors) [1 .. 10000]
    isAmicable n s = (n /= s) && (s <= 10000) && (n == arr ! s)
