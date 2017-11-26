{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Expr
       ( Expr (..)
       , eval
       , doEval
       , Eval (..)
       ) where

import           Control.Monad.Except
import qualified Data.Map             as Map


type Name = String

data Expr = Lit Int
          | Var Name
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let Name Expr Expr
    deriving (Show)

newtype Eval m a = Eval { runEval :: ExceptT String m a }
    deriving (Functor, Applicative, Monad, MonadError String)

instance MonadTrans Eval where
    lift = Eval . lift


eval :: MonadError String m => Expr -> Map.Map Name Int -> m Int
eval (Lit x) _ = return x
eval (Var x) m = case Map.lookup x m of
    Just v  -> return v
    Nothing -> throwError $
                 x ++ " is not bounded (context: " ++ show m ++ ")"
eval (Add l r) m = do
    lhs <- eval l m
    rhs <- eval r m
    return $ lhs + rhs
eval (Mul l r) m = do
    lhs <- eval l m
    rhs <- eval r m
    return $ lhs * rhs
eval e@(Div l r) m = do
    lhs <- eval l m
    rhs <- eval r m
    if rhs == 0
    then throwError $
           "Division by zero in " ++ show e ++ " (context: " ++ show m ++ ")"
    else return $ lhs * rhs
eval (Let v a e) m = do
    newval <- eval a m
    eval e (Map.insert v newval m)

doEval :: Monad m => Eval m a -> m (Either String a)
doEval ev = runExceptT (runEval ev)
