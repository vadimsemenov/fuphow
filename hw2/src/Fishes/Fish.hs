{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad.Fish
       (
       ) where

import Prelude (id, (.))

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
    -}

instance MonadFish m => Monad m where
    return  = returnFish
    m >>= f = ((\() -> m) >=> f) ()

{-
1. m >>= return === m

m >>= return === ((\() -> m) >=> return) ()
return === returnFish
((\() -> m) >=> return) () === ((\() -> m) >=> returnFish) ()
(\() -> m) >=> returnFish === (\() -> m)
(\() -> m) () === m
m >>= return === m

2. return a >>= f === f a

return a >>= f === ((\() -> return a) >=> f) ()
TODO
3. (m >>= f) >>= g === m >>= (\x -> f x >>= g)
TODO
-}

instance MonadFish f => Functor f where
    fmap f = ((>=> (return . f)) . (\() -> id)) ()

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join       = ((>=> id) . (\() -> id)) ()
--    join m     = ((\() -> m) >=> id) ()

{-
1. join . returnJoin === id

join (returnJoin a) === ((\() -> returnJoin a) >=> id) ()
returnJoin === returnFish
((\() -> returnJoin a) >=> id) () === ((\() -> returnFish a) >=> id) ()
TODO
2. join . fmap returnJoin === id
join . fmap returnJoin === 
TODO
3. join . fmap join === join . join
TODO
-}

