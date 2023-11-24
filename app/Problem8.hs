module Problem8 where

import qualified Data.ByteString as B (ByteString, filter, map, unpack)
import Data.List.Utils as L (split)
import Types (ProblemWrapper)
import Utils (wrapProblem)

windowSize :: Int
windowSize = 13

parse :: B.ByteString -> [Int]
parse = map fromIntegral . B.unpack . B.map (subtract 48) . B.filter (/= 10)

largestMult :: Int -> [Int] -> Int
largestMult n xs = maximum . scanl step m1 $ zip xs (drop n xs)
  where
    m1 = product (take n xs)
    step m (old, new) = (m `div` old) * new

_problem8 :: [Int] -> Int
_problem8 = maximum . map (largestMult windowSize) . filter ((>= windowSize) . length) . L.split [0]

problem8 :: ProblemWrapper
problem8 = wrapProblem (_problem8 . parse)
