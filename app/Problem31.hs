module Problem31 where

import Utils (intPartitions)

problem31 :: Int
problem31 = length . intPartitions 200 $ [200, 100, 50, 20, 10, 5, 2, 1]
