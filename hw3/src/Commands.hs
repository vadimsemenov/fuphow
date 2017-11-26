{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Commands
       ( VarException (..)
       , Command (..)
       , doCommand
       , declare
       , assign
       ) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as Map
import           Expr


data VarException = MultipleDeclarationException Name
                  | NotInScopeException          Name
    deriving (Show)

newtype Command m a = Command { runCommand :: ExceptT VarException (StateT Env m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState Env, MonadError VarException)

instance MonadTrans Command where
    lift = Command . lift . lift

doCommand :: Monad m => Env -> Command m a -> m (Either VarException a)
doCommand env com = evalStateT (runExceptT (runCommand com)) env


declare :: (MonadError VarException m, MonadState Env m) => Name -> Value -> m ()
declare name = doAssignment (not . Map.member name) MultipleDeclarationException name

assign :: (MonadError VarException m, MonadState Env m) => Name -> Value -> m ()
assign name = doAssignment (Map.member name) NotInScopeException name

doAssignment :: ( MonadError VarException m
                , MonadState Env m
                )
             => (Env -> Bool)
             -> (Name -> VarException)
             -> Name
             -> Value
             -> m ()
doAssignment envCheck errorProducer name val = do
    env <- get
    if envCheck env then
        modify (Map.insert name val)
    else
        throwError $ errorProducer name
