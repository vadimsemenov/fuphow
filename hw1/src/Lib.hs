module Lib
       ( plusTwo
       -- Block 1
       , order3
       , highestBitLite
       , highestBitHard
       , highestBitTrick
       , smartReplicate
       , contains
       -- Block 2
       , removeAtLite
       , removeAt
       , collectEvery
       , stringSum
       , mergeSort
       -- Block 4
       , splitOn
       , joinWith
       -- Block 5
       , maybeConcat
       , eitherConcat
       ) where

import           Control.Monad (join)
import           Data.Char     (isDigit)
import           Data.Either   (partitionEithers)
import           Data.Foldable (foldMap, foldr, toList)
import           Data.List     (splitAt)
import           Data.Maybe    (fromMaybe)


plusTwo :: [Int] -> [Int]
plusTwo = map (+2)


-- Block 1

order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 = order3' . order3''
  where
    order3' (a, b, c) = order3'' (a, min b c, max b c)
    order3'' (a, b, c) = (min a b, max a b, c)

highestBitLite :: (Real a) => a -> Integer
highestBitLite = fst . highestBitHard

highestBitHard :: (Real a) => a -> (Integer, Integer)
highestBitHard = highestBit' 1 0 (-1)
  where
    highestBit' :: (Real a) => Integer -> Integer -> Integer -> a -> (Integer, Integer)
    highestBit' cur prev deg n
        | n < fromInteger cur = (prev, deg)
        | otherwise           = highestBit' (cur * 2) cur (deg + 1) n

highestBitTrick :: (Real a) => a -> (Integer, Int)
highestBitTrick num = (last pows, length pows)
  where
    pows = takeWhile ((> num) . fromInteger) $ iterate (* 2) 1

smartReplicate :: [Int] -> [Int]
smartReplicate = foldMap $ join replicate

contains :: (Eq a) => a -> [[a]] -> [[a]]
contains e = filter $ elem e


-- Block 2

removeAtLite :: Int -> [a] -> [a]
removeAtLite n ls = snd $ removeAt n ls

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n
    | n < 0 = error "Negative index"
    | otherwise = removeAt' n
  where
    removeAt' 0 (l : ls) = (Just l, ls)
    removeAt' _ []       = (Nothing, [])
    removeAt' i (l : ls) = (fst res, l : snd res)
      where
        res = removeAt' (i - 1) ls

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery _ [] = ([], [])
collectEvery n l = (prefix ++ ps, rf ++ ss)
  where
    (prefix, remainder) = splitAt (n - 1) l
    (rf, rs) = splitAt 1 remainder
    (ps, ss) = collectEvery n rs

stringSum :: String -> Int
stringSum str = sum $ map read' $ words str
  where
    cutFirst x = (head x == '+') && isDigit (head $ tail x)
    read' x = if cutFirst x then read $ tail x else read x


mergeSort :: Ord a => [a] -> [a]
mergeSort []   = []
mergeSort [a]  = [a]
mergeSort list = merge (mergeSort fs) (mergeSort ss)
  where (fs, ss) = split list

split :: [a] -> ([a], [a])
split []          = ([], [])
split [a]         = ([a], [])
split (a : b : t) = (a : fs, b : ss)
  where (fs, ss) = split t

merge :: Ord a => [a] -> [a] -> [a]
merge [] ss             = ss
merge fs []             = fs
merge (f : fs) (s : ss) = if f < s
                          then f : merge fs (s : ss)
                          else s : merge (f : fs) ss


-- Block 4

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn sep ls = uncurry (:) $ foldr so ([], []) ls
  where
    so e (w, ts)
        | e == sep  = ([], w : ts)
        | otherwise = (e : w, ts)

joinWith :: (Foldable t, Foldable r) => a -> r (t a) -> [a]
joinWith sep ls = tail $ foldMap ((sep :) . toList) ls


-- Block 5

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat s = fromMaybe [] $ mconcat s

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat s = mconcatPair $ partitionEithers s
  where
    mconcatPair :: (Monoid a, Monoid b) => ([a], [b]) -> (a, b)
    mconcatPair (a, b) = (mconcat a, mconcat b)

