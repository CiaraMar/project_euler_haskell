module Problem23 where

import qualified Data.ByteString as B (drop)
import Data.HashSet as HS (HashSet, fromList, member)
import Data.Maybe (fromJust)
import Types (ProblemWrapper)
import Utils (cartesianProduct, parseInts, properDivisors, wrapProblem)

isAbundant :: Int -> Bool
isAbundant n = n < sum (properDivisors n)

abundantNumbers :: [Int]
abundantNumbers = filter isAbundant [1 .. 28123]

abundantSums :: HS.HashSet Int
abundantSums = HS.fromList . map sum $ cartesianProduct [abundantNumbers, abundantNumbers]

_problem23 :: Int -> Int
_problem23 n = sum . filter (not . flip HS.member abundantSums) $ [1 .. n]

problem23 :: ProblemWrapper
problem23 = wrapProblem (_problem23 . head . fromJust . parseInts (B.drop 1))
