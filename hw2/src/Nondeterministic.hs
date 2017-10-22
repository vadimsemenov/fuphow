module Nondeterministic
       ( bin
       , combinations
       , permutations
       ) where


bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = [(0 :), (1 :)] <*> bin (n - 1)

combinations :: Int -> Int -> [[Int]]
combinations _ 0 = [[]]
combinations 0 _ = []
combinations n k = combinations (n - 1) k ++ map (n :) (combinations (n - 1) (k - 1))

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (l : ls) = permutations ls >>= interleave l
  where
    interleave x []       = [[x]]
    interleave x (p : ps) = (x : p : ps) : map (p :) (interleave x ps)
