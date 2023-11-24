module Problem1 where

import Utils (sumRange)

sumMultRange :: Int -> Int -> Int
sumMultRange m n = m * sumRange (n `div` m)

problem1 :: Int
problem1 = sumMultRange 3 999 + sumMultRange 5 999 - sumMultRange 15 999
