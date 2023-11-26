module Problem26 where

import Data.List (elemIndex, maximumBy)
import Data.Ord (comparing)

problem26 :: Int
problem26 = fst $ maximumBy (comparing snd) [(n, recurringCycle n) | n <- [1 .. 999]]
  where
    recurringCycle d = remainders d 10 []
    remainders _ 0 _ = 0
    remainders d r rs =
      let r' = r `mod` d
       in case elemIndex r' rs of
            Just i -> i + 1
            Nothing -> remainders d (10 * r') (r' : rs)
