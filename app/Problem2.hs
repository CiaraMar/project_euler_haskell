module Problem2 where

fibs :: [Int]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

chunk :: Int -> [Int] -> [[Int]]
chunk n = map (take n) . iterate (drop n)

everyNth :: Int -> [Int] -> [Int]
everyNth n = map head . chunk n

problem2 :: Int
problem2 = sum . takeWhile (< 4000000) . everyNth 3 $ tail fibs
