module Problem15 where

problem15 :: Int
problem15 = fromInteger (product [21 .. 40] `div` product [2 .. 20])
