module Problem24 where

import Control.Arrow (first)
import qualified Data.Set as S (delete, elemAt, fromDistinctAscList)

baseFactorial :: Int -> [Int]
baseFactorial n =
  reverse . fst . foldr (\d (xs, m) -> first (: xs) $ m `divMod` d) ([], n) . takeWhile (<= n) . scanl1 (*) $ [1 ..]

nthPermuation :: Int -> Int -> [Int]
nthPermuation p = reverse . lastDigit . foldr step (set, []) . reverse . baseFactorial
  where
    set = S.fromDistinctAscList [0 .. p]
    step d (s, xs) = (S.delete e s, e : xs)
      where
        e = S.elemAt d s
    lastDigit (s, xs) = S.elemAt 0 s : xs

problem24 :: Int
problem24 = foldr1 (\a b -> b * 10 + a) . reverse $ nthPermuation 9 999999
