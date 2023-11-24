module Problem67 where

import Problem18 (_problem18, parse)
import Types (ProblemWrapper)
import Utils (wrapProblem)

numRows :: Int
numRows = 100

problem67 :: ProblemWrapper
problem67 = wrapProblem (_problem18 . parse numRows)
