module Parsers.ConstantFolding
       ( Let (..)
       , parse
       , formatLets
       , main
       ) where

import           Parsers.AParser            (Parser (..), char, satisfy)
import           Parsers.SExpr              (Atom (..), Ident, ident, oneOrMore,
                                             parseAtom, zeroOrMore)

import           Control.Monad              (void)
import           Data.Char                  (isSpace)
import qualified Data.HashMap.Strict.InsOrd as Map


data Let = Let { getIdent :: Ident
               , getAtoms :: [Atom]
               }
    deriving (Show, Eq)


parseLet :: Parser Let
parseLet = spaces' *> letToken *> spaces *> parseLet' <* spaces'
  where
    letToken  = void $ char 'l' *> char 'e' *> char 't'
    eq        = char '='
    eq'       = spaces' *> eq <* spaces'
    plus      = char '+'
    plus'     = spaces' *> plus <* spaces'
    parseLet' = Let <$> ident <* eq' <*> separator parseAtom plus'

parseLets :: Parser [Let]
parseLets = whitespaces' *> separator parseLet whitespaces <* whitespaces'

parse :: String -> Maybe [(Ident, Integer)]
parse input = runParser parseLets input >>= \(lets, rest) -> if not $ null rest
    then Nothing
    else parse' Map.empty lets
  where
    parse' :: Map.InsOrdHashMap Ident Integer -> [Let] -> Maybe [(Ident, Integer)]
    parse' letMap []        = Just $ Map.toList letMap
    parse' letMap (lh : ls) = eval (getAtoms lh) >>= \value ->
        parse' (Map.insert (getIdent lh) value letMap) ls
      where
        eval :: [Atom] -> Maybe Integer
        eval l = if null l then Nothing
                 else sum <$> sequence (eval' <$> l)
        eval' :: Atom -> Maybe Integer
        eval' (I i)   = Map.lookup i letMap
        eval' (N int) = Just int

formatLets :: [(Ident, Integer)] -> String
formatLets ls = unlines $ map (format (maximum $ map (length . fst) ls)) ls
  where
    format ml (i, val) = "let " ++ padId ++ " = " ++ show val
      where padId = i ++ replicate (ml - length i) ' '

-- Aux functions

separator :: Parser a -> Parser sep -> Parser [a]
separator token sep = (:) <$> token <*> zeroOrMore (sep *> token)

whitespace :: Parser Char
whitespace = satisfy isSpace

space :: Parser Char
space = satisfy (\c -> c /= '\n' && isSpace c)

spaces' :: Parser String
spaces' = zeroOrMore space

spaces :: Parser String
spaces = oneOrMore space

whitespaces' :: Parser String
whitespaces' = zeroOrMore whitespace

whitespaces :: Parser String
whitespaces = oneOrMore whitespace


main :: IO ()
main = getContents >>= \input -> putStrLn $ maybe "Failed" formatLets (parse input)
