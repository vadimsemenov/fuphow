module Expr
       ( Expr (..)
       , eval
       ) where

import qualified Data.Map as Map


data Expr = Lit Int
          | Var String
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let String Expr Expr
    deriving (Show)

eval :: Expr -> Map.Map String Int -> Either String Int
eval (Lit x) _ = Right x
eval (Var x) m = case Map.lookup x m of
    Just v  -> Right v
    Nothing -> Left ("No value for Var " ++ x ++ "; map = " ++ show m)
eval (Add l r) m = eval l m >>= \lhs ->
    eval r m >>= \rhs -> Right (lhs + rhs)
eval (Mul l r) m = eval l m >>= \lhs ->
    eval r m >>= \rhs -> Right (lhs * rhs)
eval (Div l r) m = eval l m >>= \lhs ->
    eval r m >>= \rhs -> if rhs == 0
    then Left "Division by zero"
    else Right (lhs * rhs)
eval (Let v a e) m = eval a m >>= \newval ->
    eval e (Map.insert v newval m)
