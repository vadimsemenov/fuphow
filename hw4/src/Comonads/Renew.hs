{-# LANGUAGE MultiParamTypeClasses #-}

module Comonads.Renew
       ( Renew (..)
       , MonoidAction (..)
       ) where

import           Control.Comonad (Comonad (..))
import           Data.Monoid     ((<>))


data Renew s e a = Renew (e -> a) s

class Monoid e => MonoidAction s e where
    act :: s -> e -> s

instance Functor (Renew s e) where
    fmap g (Renew f s) = Renew (g . f) s

instance MonoidAction s e => Comonad (Renew s e) where
    extract   (Renew f _) = f mempty
    extend g  (Renew f s) = Renew (g . (\e -> Renew (\e' -> f (e' <> e)) (act s e))) s
    duplicate (Renew f s) = Renew (\e -> Renew (\e' -> f (e' <> e)) (act s e)) s

{-
extract . duplicate = id
extract (duplicate (Renew f s)) =
= extract $ Renew (\e -> Renew (\e' -> f (e' <> e)) (act s e)) s
= (\e -> Renew (\e' -> f (e' <> e)) (act s e)) mempty
= Renew (\e' -> f (e' <> mempty)) (act s mempty)
= Renew (\e' -> f e') s
= Renew f s

fmap extract . duplicate = id
fmap extract (duplicate (Renew f s))
= fmap extract $ Renew (\e -> Renew (\e' -> f (e' <> e)) (act s e)) s
= Renew (\e -> extract (Renew (\e' -> f (e' <> e)) (act s e))) s
= Renew (\e -> (\e' -> f (e' <> e)) mempty) s
= Renew (\e -> f (mempty <> e)) s
= Renew (\e -> f e) s
= Renew f s

duplicate . duplicate = fmap duplicate . duplicate
fmap duplicate (duplicate (Renew f s))
= fmap duplicate (Renew (\e -> Renew (\e' -> f (e' <> e)) (act s e)) s)
= Renew (\e -> duplicate (Renew (\e' -> f (e' <> e)) (act s e))) s
= TODO
-- -}
