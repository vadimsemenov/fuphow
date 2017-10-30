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

{-  LAWS
1. fmap id ≡ id

fmap id p ≡ Parser (fmap (first id) . runParser p)              -- Definition of fmap
          ≡ Parser (fmap (\(a, c) -> (id a, c)) . runParser p)  -- Definition of first
          ≡ Parser (fmap (\(a, c) -> (a, c)) . runParser p)     -- Definition of id
          ≡ Parser (fmap id . runParser p)                      -- Definition of id
          ≡ Parser (runParser p)                                -- Identity law for
                                                    instance Functor Maybe
          ≡ p                                                   -- Definition of runParser
          ≡ id p                                                -- Definition of id

2. fmap (f . g)  ≡  fmap f . fmap g
-}

instance Functor Parser where
    fmap f p = Parser (fmap (first f) . runParser p)

{-  LAWS
1. identity
pure id <*> v ≡ v
              -- Definition of <*>
pure id <*> v ≡ Parser $ \s -> case runParser (pure id) s of
                      Just (f, rest) -> case runParser v rest of
                          Just (a, rest') -> Just (f a, rest')
                          Nothing -> Nothing
                      Nothing -> Nothing
              -- Definition of pure
              ≡ Parser $ \s -> case runParser (Parser (\s -> Just (id, s))) s of
                      Just (f, rest) -> case runParser v rest of
                          Just (a, rest') -> Just (f a, rest')
                          Nothing -> Nothing
                      Nothing -> Nothing
              -- Definition of runParser
              ≡ Parser $ \s -> case Just (id, s) of
                      Just (f, rest) -> case runParser v rest of
                          Just (a, rest') -> Just (f a, rest')
                          Nothing -> Nothing
                      Nothing -> Nothing
              -- Substitution in case
              ≡ Parser $ \s -> case runParser v s of
                      Just (a, rest') -> Just (id a, rest')
                      Nothing -> Nothing
              -- Definition of id
              ≡ Parser $ \s -> case runParser v s of
                      Just (a, rest') -> Just (a, rest')
                      Nothing -> Nothing
              -- Definition of id
              ≡ Parser $ \s -> id (runParser v s)
              -- Definition of id
              ≡ Parser $ \s -> (runParser v s)
              -- Eta reduction
              ≡ Parser $ runParser v
              -- Definition of runParser
              ≡ v

2. composition
pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
3. homomorphism
pure f <*> pure x ≡ pure (f x)
4. interchange
u <*> pure y ≡ pure ($ y) <*> u
-}

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

{- LAWS
1. m >>= return    ≡ m
             -- Definition of >>=
m >>= return ≡ Parser (\s -> runParser m s >>= (\(res, s') ->
                     (runParser (return res) s')))
             -- Definition of return
             ≡ Parser (\s -> runParser m s >>= (\(res, s') ->
                     (runParser (pure res) s')))
             -- Definition of pure
             ≡ Parser (\s -> runParser m s >>= (\(res, s') ->
                     (runParser (Parser $ \t -> Just (res, t)) s')))
             -- Definition of runParser
             ≡ Parser (\s -> runParser m s >>= (\(res, s') ->
                     (Just (res, s'))))
             -- Definition of return
             ≡ Parser (\s -> runParser m s >>= return)
             -- First law of instance Monad Maybe
             ≡ Parser (\s -> runParser m s)
             -- Eta reduction
             ≡ Parser (runParser m)
             -- Definion of runParser
             ≡ m

2. return a >>= f  ≡ f a
3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
-}

instance Monad Parser where
    return = pure
    p >>= f = Parser (runParser p >=> \(res, s') ->
        (runParser (f res) s'))
{-
    p >>= f = Parser (\s -> runParser p s >>= (\(res, s') ->
        (runParser (f res) s')))
-- -}

-- Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = (:) <$> (posInt <* char ' ') <*> (pure <$> posInt)


-- Exercise 4

instance Alternative Parser where
    empty     = Parser $ const Nothing
    p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s


-- Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
