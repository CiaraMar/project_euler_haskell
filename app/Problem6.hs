module Problem6 where

import Utils (square, sumRange, sumSquares)

problem6 :: Int
problem6 = (square . sumRange) 100 - sumSquares 100
