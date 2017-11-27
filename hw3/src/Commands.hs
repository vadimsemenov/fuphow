{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Commands
       ( CommandType (..)
       , VarException (..)
       , Command (..)
       , doCommand
       , declare
       , assign
       , evalCommand
       , evalCommands
       ) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as Map
import qualified Data.Text            as T (Text)
import qualified Data.Text.IO         as T.IO (getLine)
import           Data.Void            (Void)
import           Text.Megaparsec      (ParseError, Token, runParserT)

import           Expr
import           ExprParser           (expr)

data CommandType = Declaration Identifier Expr
                 | Assignment  Identifier Expr
                 | Print Expr
                 | Read Identifier
    deriving (Show)

data VarException = MultipleDeclarationException Identifier
                  | NotInScopeException          Identifier
                  | EvalException                ExprException
                  | ParseException               (ParseError (Token T.Text) Void)
    deriving (Show)

newtype Command m a = Command { runCommand :: ExceptT VarException (StateT Env m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState Env, MonadError VarException)

instance MonadTrans Command where
    lift = Command . lift . lift

doCommand :: Monad m => Env -> Command m a -> m (Either VarException a)
doCommand env com = evalStateT (runExceptT (runCommand com)) env


declare :: (MonadError VarException m, MonadState Env m) => Identifier -> Value -> m ()
declare name = doAssignment (not . Map.member name) MultipleDeclarationException name

assign :: (MonadError VarException m, MonadState Env m) => Identifier -> Value -> m ()
assign name = doAssignment (Map.member name) NotInScopeException name

doAssignment :: ( MonadError VarException m
                , MonadState Env m
                )
             => (Env -> Bool)
             -> (Identifier -> VarException)
             -> Identifier
             -> Value
             -> m ()
doAssignment envCheck errorProducer name val = do
    env <- get
    if envCheck env then
        modify (Map.insert name val)
    else
        throwError $ errorProducer name

evalOn :: ( MonadError VarException m
          , MonadState Env m
          , MonadIO m
          )
       => (Value -> m ())
       -> Expr
       -> m ()
evalOn func ex = do
    env <- get
    res <- doEval env $ eval ex
    case res of
        Left  e -> throwError $ EvalException e
        Right v -> func v

evalCommand :: ( MonadError VarException m
               , MonadState Env m
               , MonadIO m
               )
            => CommandType
            -> m ()
evalCommand (Declaration name ex) = evalOn (declare name) ex
evalCommand (Assignment name ex)  = evalOn (assign name) ex
evalCommand (Print ex)            = evalOn (liftIO . print) ex
evalCommand (Read name) = do
    line <- liftIO T.IO.getLine
    res  <- runParserT expr "read" line
    case res of
        Left  e -> throwError $ ParseException e
        Right r -> evalOn (modify . Map.insert name) r

evalCommands :: ( MonadError VarException m
                , MonadState Env m
                , MonadIO m
                )
             => [CommandType]
             -> m ()
evalCommands = mapM_ evalCommand
