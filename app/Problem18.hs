module Problem18 where

import qualified Data.ByteString as B (ByteString, drop)
import Data.Maybe (fromJust)

import Types (ProblemWrapper)
import Utils (parseInts, splits, wrapProblem)

numRows :: Int
numRows = 15

parse :: Int -> B.ByteString -> [[Int]]
parse n = splits [1 .. n] . fromJust . parseInts (B.drop 1)

_problem18 :: [[Int]] -> Int
_problem18 = head . foldr1 combineRows
  where
    combineRows newRow oldRow = zipWith (+) newRow (collapseRow oldRow)
    collapseRow row = zipWith max row (tail row)

problem18 :: ProblemWrapper
problem18 = wrapProblem (_problem18 . parse numRows)
