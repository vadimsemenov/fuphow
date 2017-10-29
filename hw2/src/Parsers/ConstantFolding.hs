module Parsers.ConstantFolding
       ( parse
       , formatLets
       , main
       ) where

import           Parsers.AParser     (Parser (..), char, posInt, satisfy)
import           Parsers.SExpr       (Ident, ident, oneOrMore, spaces, zeroOrMore)

import           Control.Applicative ((<|>))
import           Control.Monad       (void)
import           Data.Char           (isSpace)
import qualified Data.Map            as Map


data Let = Let { getIdent    :: Ident
               , getSummands :: [Summand]
               }
    deriving (Show, Eq)

data Summand = Const Integer | LetIdent Ident
    deriving (Show, Eq)

parseLet :: Parser Let
parseLet = spaces *> letToken *> spaces_ *> parseLet' <* spaces
  where
    spaces_ = oneOrMore (satisfy isSpace)
    letToken = void $ char 'l' *> char 'e' *> char 't'
    eq        = char '='
    eq'       = spaces *> eq <* spaces
    plus      = char '+'
    plus'     = spaces *> plus <* spaces
    summand   = (Const <$> posInt) <|> (LetIdent <$> ident)
    parseLet' = Let <$> ident <* eq' <*> ((:) <$> summand <*> zeroOrMore (plus' *> summand))

parseLets :: Parser [Let]
parseLets = zeroOrMore parseLet -- TODO: split by lines

parse :: String -> Maybe [(Ident, Integer)]
parse input = runParser parseLets input >>= \(lets, _) -> parse' Map.empty lets
  where
    parse' :: Map.Map Ident Integer -> [Let] -> Maybe [(Ident, Integer)]
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


main :: IO ()
main = getContents >>= \input -> putStrLn $ maybe "Failed" formatLets (parse input)
