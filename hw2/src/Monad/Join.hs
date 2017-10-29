{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad.Join
       ( Functor (..)
       , Monad (..)
       , MonadFish (..)
       , MonadJoin (..)
       ) where

import           Prelude ((.))

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

instance (Functor m, MonadJoin m) => Monad m where
    return = returnJoin
    m >>= f = join (fmap f m)

{-
1. m >>= return === m

m >>= return === join (fmap return m)          -- Definition of bind
             === join (fmap returnJoin m)      -- Definition of return
             === (join . (fmap returnJoin)) m  -- To composition
             === id m                          -- Second law of MonadJoin
             === m                             -- Apply id

2. return a >>= f === f a
return a >>= f === join (fmap f (return a))
               === TODO

3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
(m >>= f) >>= g === join (fmap g (m >>= f))
                === TODO
-- -}

instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnJoin
    f >=> g = join . fmap g . f

{-
1. f >=> returnFish === f

f >=> returnFish ===  join . ((fmap returnFish) . f)  -- Definition of >=>
                 ===  join . ((fmap returnJoin) . f)  -- Definition of returnFish
                 === (join . (fmap returnJoin)) . f   -- Associativity of .
                 === id                         . f   -- Second law of MonadJoin
                 === f                                -- Law of .

2. returnFish >=> f === f

returnFish >=> f ===  join . ((fmap f)  . returnFish)
                 ===  join . ((fmap f)  . returnJoin)
                 === (join .  (fmap f)) . returnJoin
                 === TODO

3. (f >=> g) >=> h === f >=> (g >=> h)

f >=> (g >=> h) ===  join . ( fmap (g >=> h                   )        . f)  -- Definition of >=>
                ===  join . ( fmap (join  .       (fmap h . g))        . f)  -- Definition of >=>
                ===  join . ((fmap  join  . (fmap (fmap h  . g)    ))  . f)  -- Composition of fmap
                ===  join . ((fmap  join  . (fmap (fmap h) . fmap g))  . f)  -- Composition of fmap
                === ( join . (fmap  join  . (fmap (fmap h) . fmap g))) . f   -- Associativity of .
                === ((join .  fmap  join) . (fmap (fmap h) . fmap g) ) . f   -- Associativity of .
                === ((join . join       ) . (fmap (fmap h) . fmap g) ) . f   -- Third law of MonadJoin
                === ( join . (join        . (fmap (fmap h) . fmap g))) . f   -- Associativity of .
                === ( join . ((join       .  fmap (fmap h)) . fmap g)) . f   -- Associativity of .

                === (join . ((join . fmap (fmap h)) . fmap g)) . f     -- Same
                === (join . ((fmap h .   join) . fmap g     )) . f     -- Fourth law of MonadJoin
                === (join . ( fmap h .  (join .  fmap g)    )) . f     -- Associativity of .
                ===  join . ((fmap h .  (join .  fmap g))      . f  )  -- Associativity of .
                ===  join . ( fmap h . ((join .  fmap g)       . f ))  -- Associatifity of .
                ===  join . ( fmap h . ( join . (fmap g        . f)))  -- Associativity of .
                ===  join . ( fmap h . ( f >= g                    ))  -- Definition of >=>
                === (f >= g) >=> h                                     -- Definition of >=>

-- -}
