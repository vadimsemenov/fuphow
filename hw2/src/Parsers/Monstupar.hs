{-# LANGUAGE TupleSections #-}

module Parsers.Monstupar
       ( Monstupar (..)
       , ParseError (..)
       , Ident
       , Atom (..)
       , SExpr (..)
       , Let (..)
       , satisfy
       , char
       , posInt
       , abParser
       , abParser_
       , intPair
       , intOrUppercase
       , oneOrMore
       , zeroOrMore
       , spaces
       , ident
       , parseAtom
       , parseSExpr
       , constantFoldingParser
       , formatLets
       ) where

import           Control.Applicative        (Alternative (..))
import           Control.Monad              (void, (>=>))
import           Data.Char                  (isAlpha, isAlphaNum, isDigit, isSpace,
                                             isUpper)
import           Data.Either.Utils          (maybeToEither)
import qualified Data.HashMap.Strict.InsOrd as Map

import           Parsers.ConstantFolding    (Let (..), formatLets)
import           Parsers.SExpr              (Atom (..), Ident, SExpr (..))


newtype ParseError = ParseError String
    deriving (Show, Eq)

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
        | null ns   = Left $ ParseError ("Cannot parse Integer from rest = '" ++ xs ++ "'")
        | otherwise = Right (rest, read ns)
      where
        (ns, rest) = span isDigit xs

abParser :: Monstupar Char (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Monstupar Char ()
abParser_ = void abParser

intPair :: Monstupar Char [Integer]
intPair = (:) <$> (posInt <* spaces) <*> (pure <$> posInt)

intOrUppercase :: Monstupar Char ()
intOrUppercase = void posInt <|> void (satisfy isUpper)

zeroOrMore :: Monstupar s a -> Monstupar s [a]
zeroOrMore parser = oneOrMore parser <|> pure []

oneOrMore :: Monstupar s a -> Monstupar s [a]
oneOrMore parser = (:) <$> parser <*> zeroOrMore parser

spaces :: Monstupar Char String
spaces = zeroOrMore $ satisfy isSpace

spaces' :: Monstupar Char String
spaces' = oneOrMore $ satisfy isSpace

ident :: Monstupar Char String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)


parseAtom :: Monstupar Char Atom
parseAtom = I <$> ident <|> N <$> posInt

parseSExpr :: Monstupar Char SExpr
parseSExpr = spaces *> parseSExpr' <* spaces
  where
    parseSExpr' = A <$> parseAtom <|> parseComb
    parseComb   = open *> spaces *> parseComb' <* spaces <* close
    parseComb'  = Comb <$> separate parseSExpr' spaces'
    open        = char '('
    close       = char ')'

separate :: Monstupar s a -> Monstupar s b -> Monstupar s [a]
separate token separator = (:) <$> token <*> zeroOrMore (separator *> token)


parseLet :: Monstupar Char Let
parseLet = spaces_ *> parseLet' <* spaces_
  where
    parseLet'   = Let <$> (letToken *> spaces_' *> ident <* eq')
                      <*> (spaces_ *> parseLetRhs <* spaces_)
    parseLetRhs = separate parseAtom plus'
    letToken    = void $ char 'l' *> char 'e' *> char 't'
    eq          = char '='
    eq'         = spaces_ *> eq <* spaces_
    plus        = char '+'
    plus'       = spaces_ *> plus <* spaces_

parseLets :: Monstupar Char [Let]
parseLets = spaces *> separate parseLet spaces' <* spaces

cfParser :: Monstupar Char (Map.InsOrdHashMap Ident Integer)
cfParser = parseLets >>= (\lets -> Monstupar $ \s -> if not $ null s
                             then Left $ ParseError "Couldn't parse entire input"
                             else maybeToEither
                                  (ParseError "Error while constant folding")
                                  ((s,) <$> eval lets Map.empty))

constantFoldingParser :: Monstupar Char [(Ident, Integer)]
constantFoldingParser = Map.toList <$> cfParser

eval :: [Let] -> Map.InsOrdHashMap Ident Integer
              -> Maybe (Map.InsOrdHashMap Ident Integer)
eval [] letMap = Just letMap
eval (l : ls) letMap = eval' l >>= (\val ->
    eval ls (Map.insert (getIdent l) val letMap))
  where
    eval' (Let _ rhs) = sum <$> sequence (eval'' <$> rhs)
    eval'' (N int) = Just int
    eval'' (I var) = Map.lookup var letMap

space_ :: Monstupar Char Char
space_ = satisfy (\c -> c /= '\n' && isSpace c)

spaces_ :: Monstupar Char String
spaces_ = zeroOrMore space_

spaces_' :: Monstupar Char String
spaces_' = oneOrMore space_
