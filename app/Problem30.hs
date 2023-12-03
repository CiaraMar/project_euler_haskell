module Problem30 where

import Utils (combosR)
import Data.List (sort)
import Control.Arrow ((&&&), first)

nthSum :: Int -> [Int] -> Int
nthSum n = sum . map (^n)

digits :: Int -> [Int]
digits a = _digits a []
   where 
      _digits 0 xs = xs
      _digits n xs = _digits q (r:xs)
         where
            (q, r) = n `divMod` 10

problem30 :: Int
problem30 = sum . map fst . filter (uncurry (==) . first (sort . digits)) . map (nthSum 5 &&& id) . combosR [2..6] $ [0..9]