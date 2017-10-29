{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ChaoticGood
       ( Identity (..)
       , Either (..)
       , Tree (..)
       , Const (..)
       , Pair (..)
       ) where


import           SnM     (Identity (..))
-- import BinarySearchTree (Tree (..))

import           Prelude ( Applicative (..)
                         , Foldable (..)
                         , Functor (..)
                         , Monoid (..)
                         , Show
                         , Traversable (..)
                         , ($)
                         , (.)
                         , (<$>)
                         )


data Either l r = Left l | Right r
    deriving (Show)

data Pair a b = Pair a b
    deriving (Show)

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show)

newtype Const a b = Const { getConst :: a }
    deriving (Show)


instance Applicative Identity where
    pure = Identity
    f <*> a = Identity (runIdentity f $ runIdentity a)

instance Functor Identity where
    fmap f = Identity . f . runIdentity

instance Traversable Identity where
    traverse f i = Identity <$> (f . runIdentity) i

instance Foldable Identity where
    foldr f acc i = f (runIdentity i) acc


instance Applicative (Either l) where
    pure                    = Right

    (Left l) <*> _          = Left l
    _ <*> (Left l)          = Left l
    (Right f) <*> (Right a) = Right (f a)

instance Functor (Either l) where
    fmap _ (Left l)  = Left l
    fmap f (Right r) = Right (f r)

instance Traversable (Either l) where
    traverse _ (Left l)  = pure (Left l)
    traverse f (Right r) = fmap Right (f r)

instance Foldable (Either l) where
    foldr _ acc (Left _)  = acc
    foldr f acc (Right r) = f r acc


instance Applicative Tree where
    pure a = Node a Leaf Leaf
    (Node f l1 r1) <*> (Node a l2 r2) = Node (f a) (l1 <*> l2) (r1 <*> r2) -- ZipTree
    _ <*> _ = Leaf

instance Functor Tree where
    fmap _ Leaf         = Leaf
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Traversable Tree where
    traverse _ Leaf         = pure Leaf
    traverse f (Node a l r) = Node <$> f a <*> traverse f l <*> traverse f r

instance Foldable Tree where
    foldr _ acc Leaf         = acc
    foldr f acc (Node a l r) = foldr f (f a (foldr f acc r)) l


instance Monoid m => Applicative (Const m) where
    pure _                  = Const mempty
    (Const a) <*> (Const b) = Const (a `mappend` b)

instance Functor (Const a) where
    fmap _ (Const a) = Const a

instance Traversable (Const a) where
    traverse _ (Const a) = pure (Const a)

instance Foldable (Const a) where
    foldr _ acc _ = acc


instance Monoid m => Applicative (Pair m) where
    pure = Pair mempty
    (Pair a f) <*> (Pair b c) = Pair (a `mappend` b) (f c)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Traversable (Pair a) where
    traverse f (Pair a b) = fmap (Pair a) (f b)

instance Foldable (Pair a) where
    foldr f acc (Pair _ b) = f b acc
