module Problem9 where

import Utils (factors)

{-
 - a^2 + b^2 = c^2
 - a + b + c = 1000
 - a = m^2 - n^2
 - b = 2mn
 - c = m^2 + n^2
 - m > n > 0
 - m * (m + n) = 500
 -}
mnToAnswer :: (Int, Int) -> Int
mnToAnswer (m, n) = (m * m - n * n) * (2 * m * n) * (m * m + n * n)

problem9 :: Int
problem9 = mnToAnswer . (\(m, mpn) -> (m, mpn - m)) . head . filter (\(m, mpn) -> mpn - m < m) . factors $ 500
