{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Utils where

import Control.Arrow ((&&&), (***), second, Arrow, first)
import Control.Monad (join)
import Control.Monad.Trans.Reader (ask)
import Data.Bits (FiniteBits, countLeadingZeros, finiteBitSize)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as BC (readInt, readInteger)
import Data.List (group, sort, subsequences, tails, foldl1')
import Data.Numbers.Primes (primes)
import Data.Array (Ix, Array, accumArray, inRange)

import Types (ProblemWrapper(ProblemWrapper))

primeFactors :: Integral int => int -> [int]
primeFactors = factor primes
  where
    factor _ 1 = []
    factor (p:ps) n =
      if r == 0
        then p : factor (p : ps) q
        else factor ps n
      where
        (q, r) = n `divMod` p
    factor _ _ = []

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x :) (subsets xs)

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs:xss) = [x : ys | x <- xs, ys <- cartesianProduct xss]

toMultiSet :: Integral int => [int] -> [(int, Int)]
toMultiSet = map (\ls -> (head ls, length ls)) . group

subMultiSets :: Integral int => [(int, Int)] -> [[(int, Int)]]
subMultiSets = cartesianProduct . map expand
  where
    expand (i, n) = [(i, m) | m <- [0 .. n]]

multiSetToList :: Integral int => [(int, Int)] -> [int]
multiSetToList = concatMap (uncurry (flip replicate))

splitRange :: Integral int => int -> [(int, int)]
splitRange n = zip [0 .. n] [n,n - 1 .. 0]

multiSetPartition :: Integral int => [(int, Int)] -> [([(int, Int)], [(int, Int)])]
multiSetPartition = map unzip . cartesianProduct . map (expand . second splitRange)
  where
    expand (_, []) = []
    expand (i, (l, r):xs) = ((i, l), (i, r)) : expand (i, xs)

factors :: Integral int => int -> [(int, int)]
factors =
  sort . map (product . multiSetToList *** product . multiSetToList) . multiSetPartition . toMultiSet . primeFactors

properDivisors :: Integral int => int -> [int]
properDivisors = reverse . tail . map snd . factors

smallestNFactors :: Integral int => Int -> int
smallestNFactors = product . zipWith (^) primes . reverse . map (subtract 1) . primeFactors

sumRange :: Int -> Int
sumRange n = (n * (n + 1)) `div` 2

sumSquares :: Int -> Int
sumSquares n = (n * (n + 1) * (2 * n + 1)) `div` 6

square :: Int -> Int
square n = n * n

interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = x : y : interleave xs ys

wrapProblem :: (B.ByteString -> Int) -> ProblemWrapper
wrapProblem f = ProblemWrapper $ do f <$> ask

-- Assumes that you start at the first int
parseIntegrals ::
     Integral int
  => (B.ByteString -> Maybe (int, B.ByteString))
  -> (B.ByteString -> B.ByteString)
  -> B.ByteString
  -> Maybe [int]
parseIntegrals readInt nextInt = _parse
  where
    _parse "" = Just []
    _parse s = do
      (n, s') <- readInt s
      fmap (n :) (_parse (nextInt s'))

parseInts :: (B.ByteString -> B.ByteString) -> B.ByteString -> Maybe [Int]
parseInts = parseIntegrals BC.readInt

parseIntegers :: (B.ByteString -> B.ByteString) -> B.ByteString -> Maybe [Integer]
parseIntegers = parseIntegrals BC.readInteger

numDigits :: Integral int => int -> int
numDigits = ceiling . logBase 10 . fromIntegral

digits :: Int -> [Int]
digits a = _digits a []
   where 
      _digits 0 xs = xs
      _digits n xs = _digits q (r:xs)
         where
            (q, r) = n `divMod` 10

fromDigits :: [Int] -> Int
fromDigits = foldl1' (\m n -> 10*m + n)

splits :: [Int] -> [a] -> [[a]]
splits [] _ = []
splits (l:ls) xs =
  let (h, xs') = splitAt l xs
   in h : splits ls xs'

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p =
  foldr
    (\x ys ->
       x :
       if p x
         then []
         else ys)
    []

-- Principle of Inclusion / Exclusion
pieCount :: Num num => ([a] -> num) -> [a] -> num
pieCount f = foldr ((\(n, s) acc -> acc + n * s) . (f &&& sgn . odd . length)) 0 . subsets
  where
    sgn True = -1
    sgn False = 1

ilog2 :: FiniteBits b => b -> Int
ilog2 x = finiteBitSize x - 1 - countLeadingZeros x

uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t5) -> (t1, t2, t3, t4) -> t5
uncurry4 f (a, b, c, d) = f a b c d

combosN :: Int -> [a] -> [[a]]
combosN 0 _ = [[]]
combosN _ [] = []
combosN n (x:xs) = map (x :) (combosN (n - 1) xs) ++ combosN n xs

combosRn :: Int -> [a] -> [[a]]
combosRn _ [] = []
combosRn 0 _ = [[]]
combosRn n ls = concatMap f (tails ls)
  where
    f xs = map (head xs :) (combosRn (n - 1) xs)

combosR :: [Int] -> [a] -> [[a]]
combosR ns ls = concatMap (`combosRn` ls) ns

intPartitions :: Int -> [Int] -> [[Int]]
intPartitions _ [] = []
intPartitions 0 _ = [[]]
intPartitions n ls = concatMap f (tails ls)
  where
    f [] = []
    f xs@(x:_) =
      if n >= x
        then map (x :) (intPartitions (n - x) xs)
        else []

hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
hist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]

mapTuple :: Arrow a => a b' c' -> a (b', b') (c', c')
mapTuple = join (***)

data Fraction a where
  Fraction :: (Integral a, Show a) => a -> a -> Fraction a

instance Show (Fraction a) where
  show (Fraction x y) = "(" ++ show x ++ "," ++ show y ++ ")"

fraction :: Fraction a -> (a, a)
fraction (Fraction x y) = (x, y)

reduceFraction :: (Integral a, Show a) => Fraction a -> Fraction a
reduceFraction = uncurry Fraction . mapTuple product . uncurry _reduce . mapTuple primeFactors . fraction
  where
    _reduce (x:xs) (y:ys) = case compare x y of 
                              EQ -> _reduce xs ys
                              LT -> first (x:) $ _reduce xs (y:ys)
                              GT -> second (y:) $ _reduce (x:xs) ys
    _reduce xs ys = (xs, ys)