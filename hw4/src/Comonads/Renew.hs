{-# LANGUAGE MultiParamTypeClasses #-}

module Comonads.Renew
       ( Renew (..)
       , MonoidAction (..)
       ) where

import           Control.Comonad (Comonad (..))


data Renew s e a = Renew (e -> a) s

class Monoid e => MonoidAction s e where
    act :: s -> e -> s

instance Functor (Renew s e) where
    fmap g (Renew f s) = Renew (g . f) s

instance MonoidAction s e => Comonad (Renew s e) where
    extract   (Renew f _) = f mempty
    extend g  (Renew f s) = Renew (g . Renew f . act s) s
    duplicate (Renew f s) = Renew (Renew f . act s) s
