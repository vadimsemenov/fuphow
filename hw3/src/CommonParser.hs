{-# LANGUAGE TypeFamilies #-}

module CommonParser
       ( ExprParser
       , spacesButNewline
       , spaceConsumer
       , spaceConsumer'
       , lineComment
       , blockComment
       , lexeme
       , symbol
       , betweenParenthesis
       , eq
       , reservedWords
       , reservedWord
       , identifier
       , value
       ) where

import           Data.Char                  (isAlphaNum, isSpace)
import           Data.Functor               (void)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char       (alphaNumChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import           Expr                       (Value)


type ExprParser m a = ParsecT Void T.Text m a


spacesButNewline :: (MonadParsec e s m, Token s ~ Char) => m ()
spacesButNewline = void $ takeWhile1P (Just "whitespace but newline") (\ch -> ch /= '\n' && isSpace ch)

spaceConsumer :: ExprParser m ()
spaceConsumer = L.space spacesButNewline lineComment blockComment

spaceConsumer' :: ExprParser m ()
spaceConsumer' = L.space space1 lineComment blockComment

lineComment :: ExprParser m ()
lineComment  = L.skipLineComment (T.pack "--")

blockComment :: ExprParser m ()
blockComment = L.skipBlockComment (T.pack "{-")  (T.pack "-}")

-- | Same parser but consumes _trailing_ whitespaces (but newlines)
lexeme :: ExprParser m a -> ExprParser m a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> ExprParser m T.Text
symbol = L.symbol spaceConsumer

-- | 'parens' parses something between parenthesis.
betweenParenthesis :: ExprParser m a -> ExprParser m a
betweenParenthesis = between (symbol $ T.pack "(") (symbol $ T.pack ")")

eq :: ExprParser m ()
eq = void $ symbol (T.pack "=")

reservedWords :: [T.Text]
reservedWords = map T.pack ["mut", "let", "in"] -- TODO: , "for", "break"]

reservedWord :: T.Text -> ExprParser m ()
reservedWord w = lexeme (string w *> notFollowedBy alphaNumChar)

identifier :: ExprParser m T.Text
identifier = (lexeme . try) (p >>= check)
  where
    p       = T.cons <$> letterChar <*> takeWhileP Nothing isAlphaNum
    check x = if x `elem` reservedWords
              then fail $ "keyword " ++ T.unpack x ++ " cannot be an identifier"
              else return x

value :: ExprParser m Value
value = lexeme L.decimal
