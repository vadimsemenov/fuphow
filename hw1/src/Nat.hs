module Nat
       ( Nat (..)
       , nat2integer
       , natEven
       , natDiv
       , natMod
       , natGcd
       ) where

data Nat = Z | S Nat
    deriving Show

natAdd :: Nat -> Nat -> Nat
natAdd n Z     = n
natAdd n (S m) = natAdd (S n) m

natMul :: Nat -> Nat -> Nat
natMul _ Z     = Z
natMul n (S m) = natAdd n $ natMul n m

natSub :: Nat -> Nat -> Nat
natSub Z _         = Z
natSub n Z         = n
natSub (S n) (S m) = natSub n m

nat2integer :: Nat -> Integer
nat2integer Z     = 0
nat2integer (S n) = 1 + nat2integer n

integer2nat :: Integer -> Nat
integer2nat n
    | n < 0     = error "Nat should be not negative"
    | n == 0    = Z
    | otherwise = S $ integer2nat $ n - 1

natEven :: Nat -> Bool
natEven Z     = True
natEven (S n) = not (natEven n)

natDiv :: Nat -> Nat -> Nat
natDiv n d = fst $ natDivRem n d

natMod :: Nat -> Nat -> Nat
natMod n d = snd $ natDivRem n d

natDivRem :: Nat -> Nat -> (Nat, Nat)
natDivRem _ 0 = error "Divizion by zero"
natDivRem n d = ndr n
  where
    ndr rest
        | rest < d  = (0, rest)
        | otherwise = let (res, rm) = ndr $ natSub rest d
                      in (natAdd res (S Z), rm)

natGcd :: Nat -> Nat -> Nat
natGcd a Z = a
natGcd a b = natGcd b (natMod a b)

instance Eq Nat where
    Z     == Z     = True
    Z     == _     = False
    _     == Z     = False
    (S n) == (S m) = n == m

instance Ord Nat where
    compare Z Z         = EQ
    compare Z _         = LT
    compare _ Z         = GT
    compare (S n) (S m) = compare n m

instance Num Nat where
    (+)         = natAdd
    (-)         = natSub
    (*)         = natMul
    abs         = id
    signum n    = if n == Z then 0 else 1
    fromInteger = integer2nat
