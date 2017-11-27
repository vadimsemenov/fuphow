module ExprParser
       ( ExprParser
       , expr
       ) where

import qualified Data.Text            as T
import           Text.Megaparsec
import           Text.Megaparsec.Expr

import           CommonParser
import           Expr                 (Expr (..))


term :: ExprParser m Expr
term  =  try (betweenParenthesis expr)
     <|> parseLit
     <|> parseVar
     <|> parseLet

parseLit :: ExprParser m Expr
parseLit = Lit <$> value

parseVar :: ExprParser m Expr
parseVar = Var <$> identifier

parseLet :: ExprParser m Expr
parseLet = betweenParenthesis $ do
    reservedWord $ T.pack "let"
    name <- identifier
    eq
    as   <- expr
    reservedWord $ T.pack "in"
    ex   <- expr
    return $ Let name as ex

expr :: ExprParser m Expr
expr = makeExprParser term operators
  where
    operators = [ [ InfixL (Mul <$ T.unpack <$> symbol (T.pack "*"))
                  , InfixL (Div <$ T.unpack <$> symbol (T.pack "/"))
                  ]
                , [ InfixL (Add <$ T.unpack <$> symbol (T.pack "+"))
                  , InfixL (Sub <$ T.unpack <$> symbol (T.pack "-"))
                  ]
                ]

