{-# LANGUAGE Rank2Types #-}

module Lenses.Basic
       ( Lens
       , Lens'
       , set
       , view
       , over
       , (.~)
       , (^.)
       , (%~)
       , _1
       , _2
       , lens
       , choosing
       , (<%~)
       , (<<%~)
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

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = setter s <$> f (getter s)

(.~) :: Lens s t a b -> b -> s -> t
l .~ b = runIdentity . l (const $ Identity b)
(^.) :: s -> Lens s t a b -> a
s ^. l = getConst $ l Const s
(%~) :: Lens s t a b -> (a -> b) -> s -> t
l %~ f = runIdentity . l (Identity . f)

-- Объединить две линзы в одну, которая работает с Either.
choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens (either (^.l1) (^.l2))
                      (either (\s1 b -> Left  $ (l1.~b) s1)
                              (\s2 b -> Right $ (l2.~b) s2))

-- Изменить цель линзы и вернуть новый результат. Постарайтесь
-- реализовать без анонимных функций и определений своих функций
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f (s ^. l), (l %~ f) s)

-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (s ^. l, (l %~ f) s)
