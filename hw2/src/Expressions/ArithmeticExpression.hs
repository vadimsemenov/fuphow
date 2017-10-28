{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module Expressions.ArithmeticExpression
       ( Expr (..)
       , ArithmeticError
       , eval
       ) where

import           Control.Applicative (liftA2)


type ArithmeticError = String

data Expr a
  where
    Const :: (Num a) => a -> Expr a
    Sum   :: (Num a) => Expr a -> Expr a -> Expr a
    Sub   :: (Num a) => Expr a -> Expr a -> Expr a
    Mul   :: (Num a) => Expr a -> Expr a -> Expr a
    Div   :: (RealFrac a) => Expr a -> Expr a -> Expr a
    Pow   :: (Num a, Integral b) => Expr a -> Expr b -> Expr a

-- deriving instance Show (Expr a)

eval :: Expr a -> Either ArithmeticError a
eval (Const a) = Right a
eval (Sum x y) = liftA2 (+) (eval x) (eval y)
eval (Sub x y) = liftA2 (-) (eval x) (eval y)
eval (Mul x y) = liftA2 (*) (eval x) (eval y)
eval (Pow x y) = eval y >>= (\t -> if t < 0
                 then Left "Negative exponent"
                 else eval x >>= \s -> Right (s ^ t))
eval (Div x y) = eval y >>= (\t -> if t == 0
                 then Left "Division by zero"
                 else eval x >>= (\s -> Right (s / t)))
