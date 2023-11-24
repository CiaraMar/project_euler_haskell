module Problem22 where

import qualified Data.ByteString as B (ByteString, filter, split, unpack)
import Data.List (sort)
import Types (ProblemWrapper)
import Utils (wrapProblem)

parse :: B.ByteString -> [B.ByteString]
parse = B.split 44 . B.filter (/= 34)

wordToInts :: B.ByteString -> [Int]
wordToInts = map (fromIntegral . subtract 64) . B.unpack

_problem22 :: [B.ByteString] -> Int
_problem22 = sum . zipWith (*) [1 ..] . map sum . sort . map wordToInts

problem22 :: ProblemWrapper
problem22 = wrapProblem (_problem22 . parse)
