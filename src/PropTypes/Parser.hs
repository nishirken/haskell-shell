{-# LANGUAGE OverloadedStrings #-}

module PropTypes.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Common (Parser)
import Data.Text (Text)
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

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curlyBrackets :: Parser a -> Parser a
curlyBrackets = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

propTypeParser :: Parser PropType
propTypeParser = undefined

propTypeStatementParser :: Parser PropTypeStatement
propTypeStatementParser = undefined
