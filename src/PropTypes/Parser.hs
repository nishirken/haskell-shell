{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PropTypes.Parser (propTypeParser, propTypeStatementsParser, objectOf, arrayOf) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Common (Parser)
import Data.Text (Text, pack)
import PropTypes.Statement (PropType (..), PropTypeStatement (..), StaticPropType (..))

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

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curlyBrackets :: Parser a -> Parser a
curlyBrackets = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

stringName :: Parser String
stringName =
  (between (symbol "'") (symbol "'") $ many alphaNumChar) <|> (between (symbol "\"") (symbol "\"") $ many alphaNumChar)

arrayOf :: Parser a -> Parser [a]
arrayOf parser = do
  xs <- squareBrackets $ many $ do
    x <- parser
    try comma <|> ((skipMany $ spaceChar <|> newline) >> (lookAhead $ symbol "]"))
    pure x
  pure xs

objectOf :: Parser a -> Parser [(Text, a)]
objectOf parser = do
  xs <- curlyBrackets $ many $ do
    fieldName <- (lexeme $ some alphaNumChar) <|> lexeme stringName
    symbol ":"
    fieldValue <- parser
    try comma <|> ((skipMany $ spaceChar <|> newline) >> (lookAhead $ symbol "}"))
    pure (pack fieldName, fieldValue)
  pure xs

type PropTypeParser = Parser PropType

anyP :: PropTypeParser
anyP = string "any" >> pure Any

boolP :: PropTypeParser
boolP = string "bool" >> pure Bool

numberP :: PropTypeParser
numberP = string "number" >> pure Number

stringP :: PropTypeParser
stringP = string "string" >> pure String

funcP:: PropTypeParser
funcP = string "func" >> pure Func

symbolP :: PropTypeParser
symbolP = string "symbol" >> pure Symbol

nodeP :: PropTypeParser
nodeP = string "node" >> pure Node

elementP :: PropTypeParser
elementP = string "element" >> pure Element

elementTypeP :: PropTypeParser
elementTypeP = string "elementType" >> pure ElementType

arrayP :: PropTypeParser
arrayP = string "array" >> pure Array

objectP :: PropTypeParser
objectP = string "object" >> pure Object

ofSomething :: Text -> Parser a -> Parser a
ofSomething keyword parser = do
  string keyword
  ofWhich <- parens parser 
  pure ofWhich

instanceOfP :: PropTypeParser
instanceOfP = InstanceOf <$> ofSomething "instanceOf" (pack <$> some alphaNumChar)

oneOfP :: PropTypeParser
oneOfP = do
  string "oneOf"
  array <- parens $ arrayOf $ stringName <|> (lexeme $ some alphaNumChar)
  pure $ OneOf $ pack <$> array

oneOfTypeP :: PropTypeParser
oneOfTypeP = do
  xs <- ofSomething "oneOfType" (arrayOf propTypeParser)
  pure $ OneOfType $ map (\PropTypeStatement{..} -> _type) xs

arrayOfP :: PropTypeParser
arrayOfP = do
  PropTypeStatement{..} <- ofSomething "arrayOf" propTypeParser
  pure $ ArrayOf _type

objectOfP :: PropTypeParser
objectOfP = do
  PropTypeStatement{..} <- ofSomething "objectOf" propTypeParser
  pure $ ObjectOf _type

shapeP :: PropTypeParser
shapeP = do
  string "shape"
  xs <- parens $ objectOf propTypeParser
  pure $ Shape $ map (\(name, PropTypeStatement{..}) -> (name, _type)) xs

exactP :: PropTypeParser
exactP = do
  string "exact"
  xs <- parens $ objectOf propTypeParser
  pure $ Exact $ map (\(name, PropTypeStatement{..}) -> (name, _type)) xs

notSupportedP :: PropTypeParser
notSupportedP = pure NotSupported

propTypeParser :: Parser PropTypeStatement
propTypeParser = do
  optional $ lexeme . try $ string "PropTypes."
  propType <-
    arrayOfP
    <|> objectOfP
    <|> arrayP
    <|> objectP
    <|> anyP
    <|> boolP
    <|> numberP
    <|> stringP
    <|> symbolP
    <|> funcP
    <|> nodeP
    <|> elementTypeP
    <|> elementP
    <|> instanceOfP
    <|> oneOfTypeP
    <|> oneOfP
    <|> shapeP
    <|> exactP
  isRequired <- optional $ char '.' >> string "isRequired"
  pure $ PropTypeStatement propType (isRequired /= Nothing)

propTypeStatementsParser :: Parser [(Text, PropTypeStatement)]
propTypeStatementsParser = do
  lexeme "static"
  lexeme "propTypes"
  symbol "="
  objectOf propTypeParser
