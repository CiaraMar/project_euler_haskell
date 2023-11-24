module Problem5 where

problem5 :: Int
problem5 = foldr1 lcm [2 .. 20]
