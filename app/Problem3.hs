module Problem3 where

import Utils (primeFactors)

problem3 :: Int
problem3 = last . primeFactors $ 600851475143
