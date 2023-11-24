module Problem13 where

import qualified Data.ByteString as B (drop)
import Data.Maybe (fromJust)

import Types (ProblemWrapper)
import Utils (numDigits, parseIntegers, wrapProblem)

wantedDigits :: Integer
wantedDigits = 10

problem13 :: ProblemWrapper
problem13 =
  wrapProblem
    (fromInteger . (\n -> n `div` (10 ^ (numDigits n - wantedDigits))) . sum . fromJust . parseIntegers (B.drop 1))
