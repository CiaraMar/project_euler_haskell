module Problem29 where

import Data.IntSet (empty, fromList, member, union)
import Utils (ilog2, pieCount, uncurry4)

occurences :: Int -> Int -> Int -> Int
occurences a b n = (b - ceiling (fromIntegral a / fromIntegral n) * n) `div` n + 1

distinctInRange :: Int -> Int -> Int -> [Int] -> Int
distinctInRange a b m = pieCount (occurences a b . foldr1 lcm . (m :))

-- [(1, 100, 6, [1,2,3,4,5]), (1, 200, 6, [2,3,4,5])...]
createRanges :: Int -> Int -> [[(Int, Int, Int, [Int])]]
createRanges a b = [(2, b, 1, [])] : [[((l - 1) * b + 1, b * l, m, [l .. m - 1]) | l <- [2 .. m]] | m <- [2 .. ilog2 a]]

rangeSums :: Int -> Int -> [Int]
rangeSums = ((scanl1 (+) . map (sum . map (uncurry4 distinctInRange))) .) . createRanges

notPowers :: Int -> [Int]
notPowers n = np empty [2 .. n]
  where
    np _ [] = []
    np s (x:xs) = x : np s' (dropWhile (`member` s') xs)
      where
        s' = s `union` fromList (takeWhile (<= n) $ iterate (x *) x)

problem29 :: Int
problem29 = sum $ map getPow $ notPowers a
  where
    a = 100 :: Int
    rs = rangeSums a a
    getPow :: Int -> Int
    getPow n = rs !! (floor l - 1)
      where
        l :: Double
        l = logBase (fromIntegral n) (fromIntegral a)
