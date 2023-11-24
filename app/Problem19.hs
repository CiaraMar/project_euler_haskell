module Problem19 where

{-
 - dow = d + m + y + c - ly(y, m)
 - 0 = 1 + m + y + 1 - ly(y, m)
 -}
centuryCodes :: [Int]
centuryCodes = [0, 5, 3, 1]

monthCodes :: [Int]
monthCodes = [6, 2, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]

yearCode :: Int -> Int
yearCode y = (y + y `div` 4) `mod` 7

isLeapYear :: Int -> Bool
isLeapYear y = (y `mod` 4) == 0 && (((y `mod` 100) /= 0) || (y `mod` 400) == 0)

leapCode :: Int -> Int -> Int
leapCode y m =
  if isLeapYear y && m <= 2
    then -1
    else 0

dow :: Int -> Int -> Int -> Int
dow y m d = (d + mc + yc + cc + leapCode y m) `mod` 7
  where
    mc = monthCodes !! (m - 1)
    yc = yearCode (y `mod` 100)
    cc = centuryCodes !! ((y `div` 100) `mod` 4)

problem19 :: Int
problem19 = length . filter (== 0) $ [dow y m 1 | y <- [1901 .. 2000], m <- [1 .. 12]]
