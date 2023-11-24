{-# LANGUAGE TupleSections #-}

module Problem11 where

import Data.Array (Array, (!), bounds, listArray)
import qualified Data.ByteString as B (ByteString, drop)
import Data.List.Utils as L (split)
import Data.Maybe (fromJust)
import Types (ProblemWrapper)
import Utils (parseInts, wrapProblem)

type I2D = (Int, Int)

type Arr2DI = Array I2D Int

windowSize :: Int
windowSize = 4

parse :: B.ByteString -> Arr2DI
parse = listArray ((1, 1), (20, 20)) . fromJust . parseInts (B.drop 1)

paths2D :: Arr2DI -> [[Int]]
paths2D arr =
  (map . map) (arr !) $
  concat
    [ [[(x, y) | x <- [sl .. el]] | y <- [sr .. er]] -- Horizantals
    , [[(x, y) | y <- [sr .. er]] | x <- [sl .. el]] -- Verticals
    , [[(x, y) | (x, y) <- zip [sx .. el] [sy .. er]] | (sx, sy) <- pos_diag_starts] -- Positive Diagonals
    , [[(x, y) | (x, y) <- zip [ex,ex - 1 .. sl] [sy .. er]] | (ex, sy) <- neg_diag_starts] -- Negative Diagonals
    ]
  where
    ((sl, sr), (el, er)) = bounds arr
    pos_diag_starts = map (sl, ) [er,er - 1 .. sr] ++ map (, sr) [sl + 1,sl + 2 .. el]
    neg_diag_starts = map (el, ) [er,er - 1 .. sr] ++ map (, sr) [el - 1,el - 2 .. sl]

-- TODO add filter for when we don't have enough remaining elements
largestMult :: Int -> [Int] -> Int
largestMult n xs = maximum . scanl step m1 $ zip xs (drop n xs)
  where
    m1 = product (take n xs)
    step m (old, new) = (m `div` old) * new

_problem11 :: Arr2DI -> Int
_problem11 =
  maximum . maximum . map (map (largestMult windowSize) . filter ((>= windowSize) . length) . L.split [0]) . paths2D

problem11 :: ProblemWrapper
problem11 = wrapProblem (_problem11 . parse)
