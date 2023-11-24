module Problem20 where

import Data.Char (digitToInt)

problem20 :: Int
problem20 = sum . map digitToInt . show $ (product [2 .. 100] :: Integer)
