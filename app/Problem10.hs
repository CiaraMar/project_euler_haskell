module Problem10 where

import Data.Numbers.Primes (wheelSieve)

problem10 :: Int
problem10 = sum $ takeWhile (< 2000000) $ wheelSieve 6
