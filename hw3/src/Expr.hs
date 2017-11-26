-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Expr
       ( Expr (..)
       , eval
       , Eval
       , runEval
       ) where

import           Control.Monad.Identity
import qualified Data.Map               as Map

type Name = String

data Expr = Lit Int
          | Var Name
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let Name Expr Expr
    deriving (Show)

{-
newtype Eval a = Eval { runEval :: a }
    deriving (Functor, Applicative, Monad)
-- -}

-- {-
type Eval = Identity
runEval :: Eval a -> a
runEval = runIdentity
-- -}

eval :: Expr -> Map.Map Name Int -> Eval Int
eval (Lit x) _ = return x
eval (Var x) m = case Map.lookup x m of
    Just v  -> return v
    Nothing -> fail $ "Var " ++ x ++ " is not bounded"
eval (Add l r) m = do
    lhs <- eval l m
    rhs <- eval r m
    return $ lhs + rhs
eval (Mul l r) m = do
    lhs <- eval l m
    rhs <- eval r m
    return $ lhs * rhs
eval (Div l r) m = do
    lhs <- eval l m
    rhs <- eval r m
    if rhs == 0
    then fail "Division by zero"
    else return $ lhs * rhs
eval (Let v a e) m = do
    newval <- eval a m
    eval e (Map.insert v newval m)
