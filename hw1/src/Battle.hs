{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Battle
       ( Creature (..)
       , attack
       , health
       , resist
       , fight
       ) where


data Creature n where
    Knight :: (Num a)  => { getKAttack :: a
                          , getKHealth :: a
                          , getKName   :: String
                          } -> Creature a
    Monster :: (Num a) => { getMAttack :: a
                          , getMHealth :: a
                          } -> Creature a

deriving instance (Show n) => Show (Creature n)

attack :: (Num a) => Creature a -> a
attack (Knight at _ _) = at
attack (Monster at _)  = at

health :: (Num a) => Creature a -> a
health (Knight _ hp _) = hp
health (Monster _ hp)  = hp

resist :: (Num a, Ord a) => Creature a -> a -> Maybe (Creature a)
resist (Knight at hp nm) punch
    | hp <= punch = Nothing
    | otherwise   = Just $ Knight at (hp - punch) nm
resist (Monster at hp) punch
    | hp <= punch = Nothing
    | otherwise   = Just $ Monster at (hp - punch)

fight :: (Num a, Ord a) => Creature a -> Creature a -> Creature a
fight assaulter defender = case resist defender (attack assaulter) of
    Just def -> fight def assaulter
    _        -> assaulter
