module Problem12 where

import Control.Arrow ((&&&), (***))
import Utils (factors, smallestNFactors)

{-
 - n * (n-1) / 2
 - fac(n) // fac(n-1)
 - 2, 3, 5 -> 3, 5, 7
 - 2^4, 3, 5
 -}
triangleGuess :: Int -> Int
triangleGuess = ceiling . sqrt . fromIntegral

problem12 :: Int
problem12 = fst . head . filter ((> 500) . snd) $ tri_factors
  where
    checkFactors (a, b) (c, d) = ((a, c), b * d)
    start = (2 *) . (`div` 4) . triangleGuess . smallestNFactors $ 500
    lf = length . factors
    mt = uncurry (*)
    tri_factors = map ((mt &&& mt . (lf *** lf)) . (id &&& subtract 1 . (2 *))) [start,start + 2 ..]
