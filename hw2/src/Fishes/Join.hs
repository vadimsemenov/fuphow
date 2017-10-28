{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad.Join
       (
       ) where

import Prelude ((.))

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

instance (Functor m, MonadJoin m) => Monad m where
    return = returnJoin
    m >>= f = join (fmap f m)

{-
1. m >>= return === m

m >>= f === join (fmap f m)
m >>= return === join (fmap return m)
return === returnJoin
join (fmap return m) === join (fmap returnJoin m)
join (fmap returnJoin m) === (join . fmap returnJoin) m
join . fmap returnJoin === id
m >>= return === m

2. return a >>= f === f a
return a >>= f === join (fmap f (return a))
return === returnJoin
join (fmap f (return a)) === join (fmap f (returnJoin a))
join (fmap f (returnJoin a)) === (join . ((fmap f) . returnJoin)) a
join . ((fmap f) . returnJoin) === (join . (fmap f)) . returnJoin
???
TODO

3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
(m >>= f) >>= g === join (fmap g (m >>= f))
join (fmap g (m >>= f)) === join (fmap g (join (fmap f m)))
join (fmap g (join (fmap f m))) === (join . (fmap g) . join . (fmap f)) m

m >>= (\x -> f x >>= g) === join (fmap (\x -> f x >>= g) m)
join (fmap (\x -> join (fmap g (f x))) m) === (join . fmap (\x -> (join . (fmap g) . f) x)) m
(join . fmap (\x -> (join . (fmap g) . f) x)) m === (join . fmap (join . (fmap g) . f)) m
join . fmap (join . (fmap g) . f) === join . ((fmap join) . (fmap (fmap g)) . fmap f)
join . ((fmap join) . (fmap (fmap g)) . fmap f) === (join . fmap join) . (fmap (fmap g)) . (fmap f)
join . join . (fmap (fmap g)) . (fmap f)
TODO

-- -}

instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnJoin
    f >=> g = join . fmap g . f

{-
1. f >=> returnFish === f

f >=> returnFish === join . (fmap returnFish) . f
returnFish === returnJoin
join . (fmap returnFish) . f === join . (fmap returnJoin) . f
join . (fmap returnJoin) . f === (join . (fmap returnJoin)) . f
join . (fmap returnJoin) === id
id . f === f
f >=> returnFish === f

2. returnFish >=> f === f

returnFish >=> f === join . (fmap g) . returnFish
returnFish === returnJoin
join . (fmap f) . returnFish === join . (fmap f) . returnJoin
TODO

3. (f >=> g) >=> h === f >=> (g >=> h)

TODO
-- -}
