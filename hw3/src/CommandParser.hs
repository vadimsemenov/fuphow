module CommandParser
       ( command
       , commands
       ) where

import           Data.Text       as T
import           Text.Megaparsec

import           Commands        (CommandType (..))
import           CommonParser
import           Expr            (Expr, Identifier)
import           ExprParser



command :: ExprParser m CommandType
command  =  parseDeclaration
        <|> parseAssignment
        <|> parsePrint
        <|> parseRead

parseDeclaration :: ExprParser m CommandType
parseDeclaration = do
    reservedWord $ T.pack "mut"
    uncurry Declaration <$> parseAssignment'

parseAssignment :: ExprParser m CommandType
parseAssignment = uncurry Assignment <$> parseAssignment'

parsePrint :: ExprParser m CommandType
parsePrint = do
    _ <- symbol $ T.pack "<"
    Print <$> expr

parseRead :: ExprParser m CommandType
parseRead = do
    _ <- symbol $ T.pack ">"
    Read <$> identifier

parseAssignment' :: ExprParser m (Identifier, Expr)
parseAssignment' = do
    name <- identifier
    eq
    ex   <- expr
    return (name, ex)

commands :: ExprParser m [CommandType]
commands = command `sepEndBy` spaceConsumer'
