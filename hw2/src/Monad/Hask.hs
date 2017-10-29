{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad.Hask
       ( Functor (..)
       , Monad (..)
       , MonadFish (..)
       , MonadJoin (..)
       , bind
       ) where

import           Prelude (id, (.))


class Functor f where
    fmap :: (a -> b) -> f a -> f b

    {-  LAWS
        1. fmap id         ≡ id
        2. fmap f . fmap g ≡ fmap (f . g)
    -}

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    {-  LAWS
        1. m >>= return    ≡ m
        2. return a >>= f  ≡ f a
        3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
    -}

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    {-  LAWS
        1. f >=> returnFish ≡ f
        2. returnFish >=> f ≡ f
        3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
    -}

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

    {-  LAWS
        1. join . returnJoin      ≡ id
        2. join . fmap returnJoin ≡ id
        3. join . fmap join       ≡ join . join
        4* join . fmap (fmap f)   ≡ fmap f . join
    -}


{-  LAWS
    1. bind return = id
    2. bind f . return = f
    3. bind g . bind f ≡ bind (bind g . f)
-}
bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = m >>= f


instance Monad m => MonadFish m where
    returnFish = return
    f >=> g    = \a -> f a >>= g

{-
1. f >=> returnFish === f

f >=> returnFish === \a -> f a >>= returnFish  -- Definition of >=>
                 === \a -> f a >>= return      -- Definition of returnFish
                 === \a -> f a                 -- First law of Monad
                 === f                         -- η-conversion
-}

instance Monad f => Functor f where
    fmap f = bind (return . f)

instance Monad m => MonadJoin m where
    returnJoin = return
    join       = bind id

{-
1. join . returnJoin === id

join . retufnJoin === bind id . returnJoin  -- Definition of join
                  === bind id . return      -- Definition of returnJoin
                  === id                    -- Second law of Monad
-}
