{-# OPTIONS_GHC -fno-warn-orphans #-}

module SnM
       ( NonEmpty (..)
       , Identity (..)
       , Name (..)
       , Endo (..)
       , Arrow (..)
       , Tree (..)
       , (<>)
       ) where

import           Data.Semigroup (Semigroup (..))
import           TreePrinters   (Tree (..))


-- NonEmpty

infixr 5 :|

data NonEmpty a = a :| [a]
    deriving Show

instance Semigroup (NonEmpty a) where
    (a :| as) <> (b :| bs) = a :| (as ++ [b] ++ bs)


-- Identity

newtype Identity a = Identity { runIdentity :: a }
    deriving Show

instance (Semigroup a) => Semigroup (Identity a) where
    a <> b = Identity $ runIdentity a <> runIdentity b

instance (Monoid a) => Monoid (Identity a) where
    mempty      = Identity mempty
    mappend a b = Identity $ runIdentity a `mappend` runIdentity b


-- Name

newtype Name = Name String
    deriving Show

instance Semigroup Name where
    na@(Name a) <> nb@(Name b)
        | null a    = nb
        | null b    = na
        | otherwise = Name (a ++ "." ++ b)

instance Monoid Name where
    mempty  = Name ""
    mappend = (<>)


-- Endo

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    a <> b = Endo $ getEndo a . getEndo b

instance Monoid (Endo a) where
    mempty  = Endo id
    mappend = (<>)


-- Arrow

newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
    f <> g = Arrow (\x -> getArrow f x <> getArrow g x)

instance Monoid    b => Monoid    (Arrow a b) where
    mempty = Arrow $ const mempty
    mappend f g = Arrow $ \x -> getArrow f x `mappend` getArrow g x


-- Tree

instance (Ord a) => Semigroup (Tree a) where
    (<>) = merge

instance (Ord a) => Monoid (Tree a) where
    mempty  = Leaf
    mappend = (<>)

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Leaf Leaf  = Leaf
merge Leaf tree  = tree
merge tree Leaf  = tree
merge left right = Node mx newLeft right
  where
    (mx, newLeft) = findMax left
    findMax Leaf              = error "could not happen"
    findMax (Node val l Leaf) = (val, l)
    findMax (Node val l r)    = let (m, nr) = findMax r in (m, Node val l nr)
