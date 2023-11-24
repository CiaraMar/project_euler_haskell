module Problem4 where

import Data.List (sortBy)

isPalindrome :: Int -> Bool
isPalindrome = (\s -> s == reverse s) . show

problem4 :: Int
problem4 = head . filter isPalindrome . sortBy (flip compare) $ [x * y | x <- [999,998 .. 100], y <- [999,998 .. 100]]
