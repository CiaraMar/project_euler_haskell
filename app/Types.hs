module Types where

import Control.Monad.Trans.Reader (Reader)
import Data.ByteString as B (ByteString)

type Problem = Reader B.ByteString Int

newtype ProblemWrapper =
  ProblemWrapper
    { unwrapProblem :: Problem
    }
