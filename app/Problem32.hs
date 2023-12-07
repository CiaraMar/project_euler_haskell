module Problem32 where
import Utils (digits, fromDigits, combosN, hist)
import Data.Array (elems)
import Control.Arrow ((&&&))
import Data.List (permutations, sort)
import Data.List.Utils (uniq)

makeCandidates :: [([Int], [Int])]
makeCandidates = [splitAt n perm | n <- [1, 2], combo <- combosN 5 [1..9], perm <- permutations combo]

candidateProduct :: ([Int], [Int]) -> [Int]
candidateProduct (a, b) = digits (fromDigits a * fromDigits b)

checkCandidate :: (([Int], [Int]), [Int]) -> Bool
checkCandidate ((a, b), c) = all (==1) . elems . hist (1,9) $ (a ++ b ++ c)

problem32 :: Int
problem32 = sum . uniq . sort $ products
   where
      products = map (fromDigits . snd) (filter checkCandidate (map (id &&& candidateProduct) makeCandidates))