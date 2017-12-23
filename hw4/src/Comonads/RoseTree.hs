{-# LANGUAGE DeriveFunctor #-}

module Comonads.RoseTree
       ( Tree (..)
       , TreeZipper (..)
       , Ancestor (..)
       , next
       , prev
       ) where

import           Control.Comonad (Comonad (..))


data Tree a = Node a [Tree a]
    deriving (Show, Functor)

instance Comonad Tree where
    extract     (Node v _)        = v
    extend f  t@(Node _ children) = Node (f t) (map (extend f) children)
    duplicate t@(Node _ children) = Node t (map duplicate children)

{-
Tree(a) = a * List(Tree(a))
Tree'(a) = List(Tree(a)) + a * List'(Tree(a)) * Tree'(a)
Tree'(a) * (1 - a * List'(Tree(a))) = List(Tree(a))
Tree'(a) = List(Tree(a)) / (1 - a * List'(Tree(a)))
Tree'(a) = List(Tree(a)) * List(a * List'(Tree(a)))
Tree'(a) = List(Tree(a)) * List(a * List^2(Tree(a)))

TreeZipper(a) = a * List(Tree(a)) * List(a * List^2(Tree(a)))
TreeZipper(a) = Tree(a) * List(a * List^2(Tree(a)))
-}

data Ancestor a = Ancestor a [Tree a] [Tree a]

data TreeZipper a = TreeZipper (Tree a) [Ancestor a]

next :: TreeZipper a -> TreeZipper a
next tz@(TreeZipper (Node _ []) []) = noNext tz
next tz@(TreeZipper current@(Node _ []) ancestors) = next' current ancestors
  where
    next' _ [] = noNext tz
    next' child (Ancestor val lhs rhs : as) = if null rhs
        then next' (Node val (child : lhs)) as
        else TreeZipper (head rhs) (Ancestor val (child : lhs) (tail rhs) : as)
next (TreeZipper (Node val children) ancestors) =
    TreeZipper (head children) (Ancestor val [] (tail children) : ancestors)

prev :: TreeZipper a -> TreeZipper a
prev tz@(TreeZipper _ []) = noNext tz
prev tz@(TreeZipper current ancestors) = prev' current ancestors
  where
    prev' _ [] = noNext tz
    prev' child (Ancestor val lhs rhs : as) = if null lhs
        then TreeZipper (Node val (child : rhs)) as
        else focusLast (head lhs) (Ancestor val (tail lhs) (child : rhs) : as)
    focusLast t@(Node _ []) as = TreeZipper t as
    focusLast (Node val children) as =
        focusLast (head children) (Ancestor val (tail children) [] : as)

noNext :: TreeZipper a -> TreeZipper a
noNext = error "There is no next TreeZipper"
-- noNext = id

instance Functor Ancestor where
    fmap f (Ancestor val lhs rhs) = Ancestor (f val) (ffmap f lhs) (ffmap f rhs)

instance Functor TreeZipper where
    fmap f (TreeZipper tree ancestors) =
      TreeZipper (fmap f tree) (ffmap f ancestors)

ffmap :: Functor f => (a -> b) -> [f a] -> [f b]
ffmap f = map (fmap f)

instance Comonad TreeZipper where
    extract (TreeZipper tree _) = extract tree
    duplicate = undefined
