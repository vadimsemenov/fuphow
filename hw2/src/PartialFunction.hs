{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators      #-}

module PartialFunction
       ( type (~>)
       , partial
       , total
       , apply
       , applyOrElse
       , withDefault
       , isDefinedAt
       , orElse
       ) where

import qualified Control.Category as Cat
import           Control.Monad    ((>=>))
import           Data.Maybe       (fromMaybe, isJust)


data a ~> b
    = Partial   (a -> Maybe b) -- a partial function
    | Defaulted (a ~> b) b     -- a partial function with a default value


partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total f = Partial $ return . f

apply :: (a ~> b) -> a -> Maybe b
apply (Partial f) arg     = f arg
apply (Defaulted f d) arg = Just $ applyOrElse f arg d

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f arg d = fromMaybe d (apply f arg)

-- | Add a default value to a partial function. If the function was already
-- defaulted, override the value with the new default.
withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault (Defaulted f _) d = Defaulted f d
withDefault f d               = Defaulted f d

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f arg = isJust $ apply f arg

-- | Create a new partial function where the domain is the combination
-- of both partial functions. Priority is given to the first partial function
-- in case of conflict.
orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f g = partial $ \arg -> case apply f arg of
    Just x  -> Just x
    Nothing -> apply g arg


instance Cat.Category (~>) where
    id = total id
    f . g = partial (apply g >=> apply f)
