module Problem28 where

sumOddSquares :: Int -> Int
sumOddSquares n = (n * (2 * n + 1) * (2 * n - 1)) `div` 3

sumOdds :: Int -> Int
sumOdds n = n * n

sumDiagonals :: Int -> Int
sumDiagonals n = 4 * sumOddSquares m - 6 * sumOdds m + 6 * m - 3
  where
    m = (n + 1) `div` 2

problem28 :: Int
problem28 = sumDiagonals 1001
