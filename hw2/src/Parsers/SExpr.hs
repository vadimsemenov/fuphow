-- | Solutions for http://www.seas.upenn.edu/~cis194/spring13/hw/11-applicative2.pdf

module Parsers.SExpr
       ( zeroOrMore
       , oneOrMore
       , spaces
       , ident
       , Ident
       , Atom (..)
       , SExpr (..)
       , parseAtom
       , parseSExpr
       ) where

import           Control.Applicative (Alternative (..), (<$>))
import           Data.Char           (isAlpha, isAlphaNum, isSpace)
import           Parsers.AParser     (Parser, char, posInt, satisfy)


-- Exercise 1

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p


-- Exercise 2

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)


-- Exercise 3

type Ident = String

data Atom = N Integer | I Ident
    deriving (Show, Eq)

data SExpr = A Atom
           | Comb [SExpr]
    deriving (Show, Eq)

parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr = spaces *> parseSExpr' <* spaces
  where
    parseSExpr'   = (A <$> parseAtom) <|> char '(' *> parseSExpr'' <* char ')'
    parseSExpr''  = Comb <$> (parseSExpr''' <|> pure [])
    parseSExpr''' = (:) <$> (spaces *> parseSExpr') <*> zeroOrMore (spaces' *> parseSExpr') <* spaces
    spaces'       = oneOrMore $ satisfy isSpace
