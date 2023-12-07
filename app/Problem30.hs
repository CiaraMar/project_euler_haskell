module Problem30 where

import Utils (combosR, digits)
import Data.List (sort)
import Control.Arrow ((&&&), first)

nthSum :: Int -> [Int] -> Int
nthSum n = sum . map (^n)

problem30 :: Int
problem30 = sum . map fst . filter (uncurry (==) . first (sort . digits)) . map (nthSum 5 &&& id) . combosR [2..6] $ [0..9]