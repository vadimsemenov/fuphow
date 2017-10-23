-- | Solutions for http://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf

module Parsers.AParser
       ( Parser (..)
       , satisfy
       , char
       , posInt
       , first
       , abParser
       , abParser_
       , intPair
       , intOrUppercase
       ) where

import           Control.Applicative (Alternative (..), (<$>))
import           Control.Monad       (void, (>=>))
import           Data.Char           (isDigit, isUpper)


-- Copypaste
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs)  -- check if x satisfies the predicate
                -- if so, return x along with the remainder
                -- of the input (that is, xs)
        | p x = Just (x, xs)
        | otherwise = Nothing -- otherwise, fail

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
        | null ns = Nothing
        | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs
-- /Copypaste


-- Exercise 1

first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)


-- Exercise 2

instance Functor Parser where
    fmap f p = Parser (fmap (first f) . runParser p)

instance Applicative Parser where
    pure a    = Parser $ \s -> Just (a, s)
    p1 <*> p2 = Parser (runParser p1 >=> (\(f, rest) ->
        (first f <$> runParser p2 rest)))
{-
    p1 <*> p2 = Parser $ \s -> case runParser p1 s of
        Just (f, rest) -> case runParser p2 rest of
            Just (a, rest') -> Just (f a, rest')
            Nothing -> Nothing
        Nothing -> Nothing
-- -}


-- Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser (Integer, Integer)
intPair = (,) <$> (posInt <* char ' ') <*> posInt


-- Exercise 4

instance Alternative Parser where
    empty     = Parser $ const Nothing
    p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s


-- Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
