module Problem32 where
import Utils (digits, fromDigits, combosN, hist, mapTuple)
import Data.Array (elems)
import Data.List (permutations, sort)
import Data.List.Utils (uniq)

makeCandidates :: [([Int], Int)]
makeCandidates = [(perm, products n perm) | n <- [1, 2], combo <- combosN 5 [1..9], perm <- permutations combo]
   where
      products n p = uncurry (*) . mapTuple fromDigits $ splitAt n p

checkCandidate :: [Int] -> Int -> Bool
checkCandidate a c = (head counts == (0 :: Int)) && all (==1) (drop 1 counts) 
   where
      counts = elems . hist (0,9) $ (a ++ digits c)

problem32 :: Int
problem32 = sum . uniq . sort . map snd . filter (uncurry checkCandidate) $ makeCandidates