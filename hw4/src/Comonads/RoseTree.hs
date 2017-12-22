{-# LANGUAGE DeriveFunctor #-}

module Comonads.RoseTree
       ( Tree (..)
       ) where

import           Control.Comonad (Comonad (..))


data Tree a = Node a [Tree a]
    deriving (Show, Functor)

instance Comonad Tree where
    extract     (Node v _)        = v
    extend f  t@(Node _ children) = Node (f t) (map (extend f) children)
    duplicate t@(Node _ children) = Node t (map duplicate children)
