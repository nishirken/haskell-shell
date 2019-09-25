{-# LANGUAGE OverloadedStrings #-}

module PropTypes.Parser (propTypeParser, propTypeStatementParser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Common (Parser)
import Data.Text (Text, pack)
import PropTypes.Statement (PropType (..), PropTypeStatement (..))

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

comma :: Parser Text
comma = symbol ","

dot :: Parser Text
dot = symbol "."

required :: Parser Text
required = lexeme "isRequired"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curlyBrackets :: Parser a -> Parser a
curlyBrackets = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

stringName :: Parser String
stringName =
  (between (symbol "'") (symbol "'") $ many alphaNumChar) <|> (between (symbol "\"") (symbol "\"") $ many alphaNumChar)

arrayParser :: Parser [Text]
arrayParser = do
  xs <- squareBrackets $ many $ do
    x <- stringName <|> (lexeme $ some alphaNumChar)
    c <- optional comma
    case c of
      (Just y) -> pure x
      Nothing -> lookAhead $ symbol "]" >> pure x
  pure $ pack <$> xs

type PropTypeParser = Parser PropType

anyP :: PropTypeParser
anyP = string "any" >> pure Any

funcP:: PropTypeParser
funcP = string "func" >> pure Func

ofSomething :: Text -> Parser Text
ofSomething keyword = do
  string keyword
  ofWhich <- parens $ some alphaNumChar 
  pure $ pack ofWhich

instanceOfP :: PropTypeParser
instanceOfP = InstanceOf <$> ofSomething "instanceOf"

oneOfP :: PropTypeParser
oneOfP = do
  string "oneOf"
  array <- parens arrayParser
  pure $ OneOf array

-- oneOfType :: PropTypeParser
-- oneOfType = do
--   string "oneOfType"
--   array <- parens $ 

propTypeParser :: PropTypeParser
propTypeParser = do
  optional $ lexeme . try $ string "PropTypes."
  propType <-
    anyP
    <|> funcP
    <|> instanceOfP
    <|> oneOfP
  optional $ dot >> required
  pure propType

propTypeStatementParser :: Parser PropTypeStatement
propTypeStatementParser = undefined
