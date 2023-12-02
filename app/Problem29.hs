module Problem29 where

import Control.Arrow ((&&&), first)
import Utils (subsets)

g a b m =
  foldr
    (\(n, s) acc ->
       if s
         then acc - n
         else acc + n)
    0 .
  map (first (f a b) . (foldr1 lcm &&& even . length) . (m :)) . subsets
