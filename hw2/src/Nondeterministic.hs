module Nondeterministic
       ( bin
       , combinations
       , permutations
       ) where


bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = (:) <$> [0, 1] <*> bin (n - 1)
-- bin = (flip replicate) [0, 1]

combinations :: Int -> Int -> [[Int]]
combinations n = comb 1
  where
    comb from k
        | k == 0    = [[]]
        | otherwise = [from .. (n - k + 1)] >>= \first ->
            (first :) <$> comb (first + 1) (k - 1)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (l : ls) = permutations ls >>= interleave l
  where
    interleave x []       = [[x]]
    interleave x (p : ps) = (x : p : ps) : ((p :) <$> interleave x ps)
