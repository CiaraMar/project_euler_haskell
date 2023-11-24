module Problem7 where

import Data.Numbers.Primes

problem7 :: Int
problem7 = wheelSieve 6 !! 10000
