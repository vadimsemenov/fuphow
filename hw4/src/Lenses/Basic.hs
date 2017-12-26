{-# LANGUAGE Rank2Types #-}

module Lenses.Basic
       ( Lens
       , Lens'
       , set
       , view
       , over
       , _1
       , _2
       ) where

import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (..))


type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

-- set :: Functor f => ((a -> f a) -> s -> f s) -> a -> s -> s
set  :: Lens' s a -> a -> s -> s         -- set    value (setter)
set l e s = runIdentity $ l (const $ Identity e) s

-- view :: Functor f => ((a -> f a) -> s -> f s) -> s -> s
view :: Lens' s a -> s -> a              -- lookup value (getter)
view l s = getConst $ l Const s

-- over :: Functor f => ((a -> f a) -> s -> f s) -> (a -> a) -> s -> s
over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over l f s = runIdentity $ l (Identity . f) s

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\y -> (y, x)) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (\y -> (x, y)) <$> f a
