module Problem23 where

import Data.HashSet as HS (HashSet, fromList, member)
import Utils (cartesianProduct, properDivisors)

isAbundant :: Int -> Bool
isAbundant n = n < sum (properDivisors n)

abundantNumbers :: [Int]
abundantNumbers = filter isAbundant [1 .. 28123]

abundantSums :: HS.HashSet Int
abundantSums = HS.fromList . map sum $ cartesianProduct [abundantNumbers, abundantNumbers]

problem23 :: Int
problem23 = sum . filter (not . flip HS.member abundantSums) $ [1 .. 28123]
