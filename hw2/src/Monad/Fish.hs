{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad.Fish
       ( Functor (..)
       , Monad (..)
       , MonadFish (..)
       , MonadJoin (..)
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

instance MonadFish m => Monad m where
    return  = returnFish
    m >>= f = (id >=> f) m

{-
1. m >>= return === m

m >>= return === (id >=> return) m      -- Definition of bind
             === (id >=> returnFish) m  -- Definition of return
             === id m                   -- First law of MonadFish
             === m                      -- Definition of id

2. return a >>= f === f a

return a >>= f === (id >=> f) (return a)
               === ((id >=> f) . return) a
               === TODO

3. (m >>= f) >>= g === m >>= (\x -> f x >>= g)
TODO
-}

instance MonadFish f => Functor f where
    fmap f = id >=> (returnFish . f)

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join       = id >=> id

{-
1. join . returnJoin === id

join . returnJoin === (id >=> id) . returnJoin
                  === (id >=> id) . returnFish
                  === (>=> id) . id . returnFish
                  === TODO

2. join . fmap returnJoin === id

join . fmap returnJoin === (id >=> id) . fmap returnJoin
                       === (id >=> id) . fmap returnFish
                       === (id >=> id) . (id >=> (returnFish . returnFish))
                       === TODO
3. join . fmap join === join . join

join . fmap join === join . (id >=> (returnFish . join))
                 === join . (id >=> (returnFish . (id >=> id)))
                 === TODO

lemma:
returnFish . (id >=> id) === id
returnFish . (id >=> id) === TODO
-}

