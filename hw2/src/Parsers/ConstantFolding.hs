module Parsers.ConstantFolding
       ( parse
       , formatLets
       , main
       ) where

import           Parsers.AParser            (Parser (..), char, posInt, satisfy)
import           Parsers.SExpr              (Ident, ident, oneOrMore, zeroOrMore)

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.Char                  (isSpace)
import qualified Data.HashMap.Strict.InsOrd as Map


data Let = Let { getIdent    :: Ident
               , getSummands :: [Summand]
               }
    deriving (Show, Eq)

data Summand = Const Integer | LetIdent Ident
    deriving (Show, Eq)


parseLet :: Parser Let
parseLet = spaces' *> letToken *> spaces *> parseLet' <* spaces'
  where
    letToken = void $ char 'l' *> char 'e' *> char 't'
    eq        = char '='
    eq'       = spaces' *> eq <* spaces'
    plus      = char '+'
    plus'     = spaces' *> plus <* spaces'
    summand   = (Const <$> posInt) <|> (LetIdent <$> ident)
    parseLet' = Let <$> ident <* eq' <*> separator summand plus'

parseLets :: Parser [Let]
parseLets = whitespaces' *> separator parseLet whitespaces <* whitespaces'

parse :: String -> Maybe [(Ident, Integer)]
parse input = runParser parseLets input >>= \(lets, rest) -> if not $ null rest
    then Nothing
    else parse' Map.empty lets
  where
    parse' :: Map.InsOrdHashMap Ident Integer -> [Let] -> Maybe [(Ident, Integer)]
    parse' letMap []        = Just $ Map.toList letMap
    parse' letMap (lh : ls) = eval (getSummands lh) >>= \value ->
        parse' (Map.insert (getIdent lh) value letMap) ls
      where
        eval :: [Summand] -> Maybe Integer
        eval l = if null l then Nothing
                 else foldr (\s acc -> (+) <$> eval' s <*> acc) (Just 0) l

        eval' :: Summand -> Maybe Integer
        eval' (LetIdent i) = Map.lookup i letMap
        eval' (Const int)  = Just int

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
