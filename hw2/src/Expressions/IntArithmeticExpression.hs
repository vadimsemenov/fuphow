module Expressions.IntArithmeticExpression
       ( IntExpr (..)
       , IntArithmeticError
       , evalInt
       ) where

import           Control.Applicative (liftA2)


type IntArithmeticError = String

data IntExpr = Const Int
             | Sum IntExpr IntExpr
             | Sub IntExpr IntExpr
             | Mul IntExpr IntExpr
             | Div IntExpr IntExpr
             | Pow IntExpr IntExpr
    deriving (Show)

evalInt :: IntExpr -> Either IntArithmeticError Int
evalInt (Const a) = Right a
evalInt (Sum x y) = liftA2 (+) (evalInt x) (evalInt y)
evalInt (Sub x y) = liftA2 (-) (evalInt x) (evalInt y)
evalInt (Mul x y) = liftA2 (*) (evalInt x) (evalInt y)
evalInt (Pow x y) = evalInt y >>= (\t -> if t < 0
                 then Left "Negative exponent"
                 else evalInt x >>= \s -> Right (s ^ t))
{-
                 else evalInt x >>= \s -> if s == 0 && t == 0
                 then Left "0^0 is NaN"
                 else Right (s ^ t)
-- -}
evalInt (Div x y) = evalInt y >>= (\t -> if t == 0
                 then Left "Division by zero"
                 else evalInt x >>= (\s -> Right (s `div` t)))
