module Problem26 where

longDivision :: Int -> Int -> [Int]
longDivision n d = map fst . takeWhile ((/= 0) . snd) . iterate step $ ((0, n) :: (Int, Int))
  where
    step = (

cycleLength :: Eq a => [a] -> Int
cycleLength = undefined

problem26 :: Int
problem26 = maximum . map (cycleLength . longDivision 1) $ [2 .. 1000]
