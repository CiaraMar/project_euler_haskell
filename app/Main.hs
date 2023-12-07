{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Monad.Trans.Reader (runReader)
import Data.ByteString as B (readFile)

-- TODO use Template Haskell for Meta Programming
import Problem1 as P1 (problem1)
import Problem10 as P10 (problem10)
import Problem11 as P11 (problem11)
import Problem12 as P12 (problem12)
import Problem13 as P13 (problem13)
import Problem14 as P14 (problem14)
import Problem15 as P15 (problem15)
import Problem16 as P16 (problem16)
import Problem17 as P17 (problem17)
import Problem18 as P18 (problem18)
import Problem19 as P19 (problem19)
import Problem2 as P2 (problem2)
import Problem20 as P20 (problem20)
import Problem21 as P21 (problem21)
import Problem22 as P22 (problem22)
import Problem23 as P23 (problem23)
import Problem24 as P24 (problem24)
import Problem25 as P25 (problem25)
import Problem26 as P26 (problem26)
import Problem27 as P27 (problem27)
import Problem28 as P28 (problem28)
import Problem29 as P29 (problem29)
import Problem3 as P3 (problem3)
import Problem30 as P30 (problem30)
import Problem31 as P31 (problem31)
import Problem32 as P32 (problem32)
import Problem4 as P4 (problem4)
import Problem5 as P5 (problem5)
import Problem6 as P6 (problem6)
import Problem67 as P67 (problem67)
import Problem7 as P7 (problem7)
import Problem8 as P8 (problem8)
import Problem9 as P9 (problem9)
import System.Environment (getArgs)
import Types (ProblemWrapper(ProblemWrapper))

class Problems a where
  evaluate :: Int -> a -> IO Int

instance Problems Int where
  evaluate _ = pure

instance Problems ProblemWrapper where
  evaluate n (ProblemWrapper r) = do
    contents <- B.readFile ("input/Problem" ++ show n ++ ".txt")
    return $ runReader r contents

data AProblems =
  forall a. Problems a =>
            AProblems a

evalProblem :: Int -> AProblems -> IO Int
evalProblem n (AProblems x) = evaluate n x

problems :: [AProblems]
problems =
  [ AProblems P1.problem1
  , AProblems P2.problem2
  , AProblems P3.problem3
  , AProblems P4.problem4
  , AProblems P5.problem5
  , AProblems P6.problem6
  , AProblems P7.problem7
  , AProblems P8.problem8
  , AProblems P9.problem9
  , AProblems P10.problem10
  , AProblems P11.problem11
  , AProblems P12.problem12
  , AProblems P13.problem13
  , AProblems P14.problem14
  , AProblems P15.problem15
  , AProblems P16.problem16
  , AProblems P17.problem17
  , AProblems P18.problem18
  , AProblems P19.problem19
  , AProblems P20.problem20
  , AProblems P21.problem21
  , AProblems P22.problem22
  , AProblems P23.problem23
  , AProblems P24.problem24
  , AProblems P25.problem25
  , AProblems P26.problem26
  , AProblems P27.problem27
  , AProblems P28.problem28
  , AProblems P29.problem29
  , AProblems P30.problem30
  , AProblems P31.problem31
  , AProblems P32.problem32
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , AProblems P67.problem67
  ]

main :: IO ()
main = do
  args <- getArgs
  let n = read (last args) :: Int
  answer <- evalProblem n (problems !! (n - 1))
  print answer
