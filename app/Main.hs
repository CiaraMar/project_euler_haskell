{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Monad.Trans.Reader (runReader)
import Criterion.Main (bench, bgroup, defaultMain, env, whnfAppIO)
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
import Problem3 as P3 (problem3)
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

setupEnv :: IO (Int, Int -> IO Int)
setupEnv = do
  args <- getArgs
  let n = read (last args) :: Int
  return (n, \m -> evalProblem m (problems !! (m - 1)))

main :: IO ()
main = do
  (n, problem) <- setupEnv
  answer <- problem n
  print answer
  defaultMain [env setupEnv $ \ ~(n, problem) -> bgroup "main" [bench ("problem " ++ show n) $ whnfAppIO problem n]]
