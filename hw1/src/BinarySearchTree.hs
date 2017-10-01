{-# OPTIONS_GHC -fno-warn-orphans #-}

module BinarySearchTree
       ( null
       , toList
       , size
       , member
       , insert
       , fromList
       , Tree (..)
       ) where

import           TreePrinters (Tree (..))
import           Data.Monoid (Sum (..))
import           Data.Foldable (Foldable (null, toList))

size :: Tree a -> Int
size = getSum . (foldMap (const $ Sum 1))

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member key (Node val left right)
  | val == key = True
  | val < key  = member key right
  | otherwise  = member key left

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf x = Node x Leaf Leaf
insert (Node val left right) x
  | val == x  = Node val left right
  | val < x   = Node val left (insert right x)
  | otherwise = Node val (insert left x) right

-- | Works only for Ord
-- toList . fromList ≡ sort
fromList :: Ord a => [a] -> Tree a
fromList []       = Leaf
fromList (x : xs) = Node x (fromList $ filter (< x) xs) (fromList $ filter (> x) xs)

instance Foldable Tree where
  foldr _ acc Leaf                  = acc
  foldr f acc (Node val left right) = foldr f (f val (foldr f acc right)) left

  foldMap _ Leaf         = mempty
  foldMap f (Node v l r) = foldMap f l `mappend` f v `mappend` foldMap f r
