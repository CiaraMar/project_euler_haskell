{-# LANGUAGE OverloadedStrings #-}

module Problem17 where

import qualified Data.ByteString as B (ByteString, length)

ones :: [B.ByteString]
ones = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

teens :: [B.ByteString]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [B.ByteString]
tens = ["twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]

onesLengths :: [Int]
onesLengths = map B.length ones

teensLengths :: [Int]
teensLengths = map B.length teens

tensLengths :: [Int]
tensLengths = map B.length tens

-- Ones:                      Ones(Tens)    100s(Ones)    Ones(100s)
onesCount :: Int
onesCount = sum onesLengths * (1 + length tens + 10) * 10

tensCount :: Int
tensCount = sum tensLengths * (1 + length ones) * 10

teensCount :: Int
teensCount = sum teensLengths * 10

hundredsCount :: Int
hundredsCount = 7 * 900 + 3 * 891

problem17 :: Int
--         1000   
problem17 = 11 + onesCount + teensCount + tensCount + hundredsCount
