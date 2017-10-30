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

{- Functor laws
1. fmap id         ≡ id
2. fmap f . fmap g ≡ fmap (f . g)
-}

{- Applicative laws
1. identity
pure id <*> v ≡ v
2. composition
pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
3. homomorphism
pure f <*> pure x ≡ pure (f x)
4. interchange
u <*> pure y ≡ pure ($ y) <*> u
-}

{- Foldable laws
1. fold ≡ foldMap id
2. foldMap f ≡ fold . fmap f
-}

{- Traversable laws
1. naturality
t . traverse f === traverse (t . f) for every applicative transformation t
2. identity
traverse Identity === Identity
3. composition
traverse (Compose . fmap g . f) === Compose . fmap (traverse g) . traverse f
-}

{- u <*> pure y ≡ pure ($ y) <*> u
u <*> pure y === Identity (runIdentity u $ runIdentity (pure y))
             === Identity (runIdentity u $ runIdentity (Identity y))
             === Identity (runIdentity u $ y)
             === Identity (($ y) (runIdentity u))
             === Identity (runIdentity (pure ($ y)) (runIdentity u))
             === pure ($ y) <*> u
-}
instance Applicative Identity where
    pure = Identity
    f <*> a = Identity (runIdentity f $ runIdentity a)

{- fmap id === id
fmap id === Identity . id . runIdentity
        === Identity . runIdentity
        === id
-}
instance Functor Identity where
    fmap f = Identity . f . runIdentity

{- t . traverse f === traverse (t . f) for every applicative transformation t
t (traverse f i) === t (Identity <$> (f . runIdentity) i)
                 === t (Identity <$> (f . runIdentity) i)
                 === t ((Identity . Identity . runIdentity) ((f . runIdentity) i))
                 === t (Identity (Identity (runIdentity ((f . runIdentity) i))))
-}
{- traverse Identity === Identity
traverse Identity i === Identity <$> (Identity . runIdentity) i
                    === Identity <$> i
                    === Identity . Identity . (runIdentity i)
                    === Identity (Identity (runIdentity i))
                    === Identity i
-}
instance Traversable Identity where
    traverse f i = Identity <$> (f . runIdentity) i

{- foldMap f ≡ fold . fmap f
foldMap f i === foldr (mappend . f) mempty i
            === (mappend . f) (runIdentity i) mempty
            === mappend (f (runIdentity i)) mempty
            === mappend (runIdentity (Identity (f (runIdentity i)))) mempty
            === mappend (runIdentity (fmap f i)) mempty
            === (mappend . id) (runIdentity (fmap f i)) mempty
            === foldr (mappend . id) mempty (fmap f i)
            === foldMap id (fmap f i)
            === fold (fmap f i)
            === (fold . fmap f) i
-}
instance Foldable Identity where
    foldr f acc i = f (runIdentity i) acc
--  foldMap f = foldr (mappend . f) mempty
--  fold = foldMap id


instance Applicative (Either l) where
    pure                    = Right

    (Left l) <*> _          = Left l
    _ <*> (Left l)          = Left l
    (Right f) <*> (Right a) = Right (f a)

{- fmap (f . g) === fmap f . fmap g
/Left
fmap (f . g) (Left l) === Left l
                      === fmap g (Left l)
                      === fmap f (fmap g (Left l))
                      === (fmap f . fmap g) (Left l)
/Right
fmap (f . g) (Right r) === Right ((f . g) r)
                       === Right (f (g r))
                       === fmap f (Right (g r))
                       === fmap f . fmap g (Right r)
-}
instance Functor (Either l) where
    fmap _ (Left l)  = Left l
    fmap f (Right r) = Right (f r)

instance Traversable (Either l) where
    traverse _ (Left l)  = pure (Left l)
    traverse f (Right r) = fmap Right (f r)

{- fold ≡ foldMap id
By definition
-}
instance Foldable (Either l) where
    foldr _ acc (Left _)  = acc
    foldr f acc (Right r) = f r acc
--  foldMap f = foldr (mappend . f) mempty
--  fold = foldMap id

{- pure id <*> v ≡ v
/v === Leaf
pure id <*> v === Leaf
              === v
/v === Node val left right
pure id <*> (Node val left right) === p@(Node id Leaf Leaf) <*> (Node val left right)
                                  === Node (id val) (p <*> left) (p <*> right)
                                  === Node (val) (p <*> left) (p <*> right)
                                  === Node (val) (left) (right)
-}
instance Applicative Tree where
    pure a = Node a Leaf Leaf
    t@(Node f _ _) <*> (Node a l r) = Node (f a) (t <*> l) (t <*> r)
    _ <*> _ = Leaf

instance Functor Tree where
    fmap _ Leaf         = Leaf
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Traversable Tree where
    traverse _ Leaf         = pure Leaf
    traverse f (Node a l r) = Node <$> f a <*> traverse f l <*> traverse f r

{- foldMap f ≡ fold . fmap f
foldMap f === foldr (mappend . f) mempty
/Leaf
foldr (mappend . f) mempty Leaf === mempty
                                === foldMap id mempty Leaf
                                === foldMap id mempty (fmap f Leaf)
                                === fold (fmap f Leaf)
                                === (fold . fmap f) Leaf
/Right
foldr (mappend . f) mempty (Node a l r) === TODO
-}
instance Foldable Tree where
    foldr _ acc Leaf         = acc
    foldr f acc (Node a l r) = foldr f (f a (foldr f acc r)) l
--  foldMap f = foldr (mappend . f) mempty
--  fold = foldMap id

{- pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
pure (.) <*> u <*> v <*> w === Const mempty <*> u <*> v <*> w
                           === Const mempty <*> Const ux <*> Const vx <*> Const wx
                           === Const (mempty `mappend` ux) <*> Const vx <*> Const wx
                           === Const ux <*> Const vx <*> Const wx
                           === Const (ux `mappend` vx) <*> Const wx
                           === Const (ux `mappend` vx `mappend` wx)
                           === Const (ux `mappend` (vx `mappend` wx))
                           === Const ux <*> (vx `mappend` wx)
                           === Const ux <*> (Const vx <*> Const wx)
                           === u <*> (v <*> w)
-}
instance Monoid m => Applicative (Const m) where
    pure _                  = Const mempty
    (Const a) <*> (Const b) = Const (a `mappend` b)

instance Functor (Const a) where
    fmap _ (Const a) = Const a

instance Traversable (Const a) where
    traverse _ (Const a) = pure (Const a)

instance Foldable (Const a) where
    foldr _ acc _ = acc


{- pure f <*> pure x ≡ pure (f x)
pure f <*> pure x === pure (f x)
pure f <*> pure x === (Pair mempty f) <*> (Pair mempty x)
                  === Pair (mempty `mappend` mempty) (f x)
                  === Pair mempty (f x)
                  === pure (f x)
--}
instance Monoid m => Applicative (Pair m) where
    pure = Pair mempty
    (Pair a f) <*> (Pair b c) = Pair (a `mappend` b) (f c)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

{- t . traverse f === traverse (t . f) for every applicative transformation t
t (traverse f (Pair a b)) === t (fmap (Pair a) (f b))
                          -- fmap f x === pure f <*> x
                          === t (pure (Pair a) <*> (f b))
                          -- t (x <*> y) = t x <*> t y
                          === t (pure (Pair a)) <*> t (f b)
                          -- t (pure x) === pure x
                          === pure (Pair a) <*> t (f b)
                          === fmap (Pair a) (t (f b))
                          === fmap (Pair a) ((t . f) b)
                          === traverse (t . f) (Pair a, b)
-}
instance Traversable (Pair a) where
    traverse f (Pair a b) = fmap (Pair a) (f b)

instance Foldable (Pair a) where
    foldr f acc (Pair _ b) = f b acc
