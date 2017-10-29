module Parsers.Monstupar
       ( Monstupar (..)
       , ParseError
       , satisfy
       , char
       , posInt
       , abParser
       , abParser_
       , intOrUppercase
       ) where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (void, (>=>))
import           Data.Char           (isDigit, isUpper)


newtype ParseError = ParseError String
    deriving (Show)

newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a) }


instance Functor (Monstupar s) where
    fmap f p = Monstupar (fmap (second f) . runParser p)

instance Applicative (Monstupar s) where
    pure a  = Monstupar $ \s -> Right (s, a)
    f <*> p = Monstupar (runParser f >=> (\(s', g) ->
        runParser p s' >>= \(rest, a) -> Right (rest, g a)))

instance Alternative (Monstupar s) where
    empty   = Monstupar $ \_ -> Left $ ParseError "Empty"
    p <|> r = Monstupar $ \s -> alt s $ runParser p s
      where
        alt t (Left _) = runParser r t
        alt _ result   = result

instance Monad (Monstupar s) where
    return  = pure
    p >>= f = Monstupar (runParser p >=> (\(s', res) ->
        runParser (f res) s'))


second :: (a -> b) -> (c, a) -> (c, b)
second f (x, y) = (x, f y)

satisfy :: (s -> Bool) -> Monstupar s s
satisfy predicate = Monstupar f
  where
    f [] = Left $ ParseError "Unexpected EOF in satisfy"
    f (x : xs)
       | predicate x = Right (xs, x)
       | otherwise   = Left $ ParseError "Doesn't satisfy predicate"

char :: Char -> Monstupar Char Char
char c = satisfy (== c)

posInt :: Monstupar Char Integer
posInt = Monstupar f
  where
    f xs
        | null ns   = Left $ ParseError "Unexpected EOF in posInt"
        | otherwise = Right (rest, read ns)
      where
        (ns, rest) = span isDigit xs

abParser :: Monstupar Char (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Monstupar Char ()
abParser_ = void abParser

intOrUppercase :: Monstupar Char ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
