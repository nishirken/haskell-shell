{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PropTypes.Parser (propTypeParser, propTypeStatementsParser, objectOf, arrayOf, lexeme, symbol) where

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

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curlyBrackets :: Parser a -> Parser a
curlyBrackets = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

stringName :: Parser String
stringName = do
  x <- (between (symbol "'") (symbol "'") $ many alphaNumChar) <|> (between (symbol "\"") (symbol "\"") $ many alphaNumChar)
  pure $ "'" <> x <> "'"

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

type PropTypeParser = Parser PropTypeStatement

anyP :: PropTypeParser
anyP = do
  string "any"
  r <- requiredP
  pure $ PropTypeStatement Any r

boolP :: PropTypeParser
boolP = do
  string "bool"
  r <- requiredP
  pure $ PropTypeStatement Bool r

numberP :: PropTypeParser
numberP = do
  string "number"
  r <- requiredP
  pure $ PropTypeStatement Number r

stringP :: PropTypeParser
stringP = do
  string "string"
  r <- requiredP
  pure $ PropTypeStatement String r

funcP:: PropTypeParser
funcP = do
  string "func"
  r <- requiredP
  pure $ PropTypeStatement Func r

symbolP :: PropTypeParser
symbolP = do
  string "symbol"
  r <- requiredP
  pure $ PropTypeStatement Symbol r

nodeP :: PropTypeParser
nodeP = do
  string "node"
  r <- requiredP
  pure $ PropTypeStatement Node r

elementP :: PropTypeParser
elementP = do
  string "element"
  r <- requiredP
  pure $ PropTypeStatement Element r

elementTypeP :: PropTypeParser
elementTypeP = do
  string "elementType"
  r <- requiredP
  pure $ PropTypeStatement ElementType r

arrayP :: PropTypeParser
arrayP = do
  string "array"
  r <- requiredP
  pure $ PropTypeStatement Array r

objectP :: PropTypeParser
objectP = do
  string "object"
  r <- requiredP
  pure $ PropTypeStatement Object r

ofSomething :: Text -> Parser a -> Parser a
ofSomething keyword parser = do
  string keyword
  ofWhich <- parens parser 
  pure ofWhich

instanceOfP :: PropTypeParser
instanceOfP = do
  x <- ofSomething "instanceOf" (pack <$> some alphaNumChar)
  r <- requiredP
  pure $ PropTypeStatement (InstanceOf x) r

oneOfP :: PropTypeParser
oneOfP = do
  string "oneOf"
  array <- parens $ arrayOf $ stringName <|> (lexeme $ some alphaNumChar)
  r <- requiredP
  pure $ PropTypeStatement (OneOf $ pack <$> array) r

oneOfTypeP :: PropTypeParser
oneOfTypeP = do
  xs <- ofSomething "oneOfType" (arrayOf propTypeParser)
  r <- requiredP
  pure $ PropTypeStatement (OneOfType xs) r

arrayOfP :: PropTypeParser
arrayOfP = do
  x <- ofSomething "arrayOf" propTypeParser
  r <- requiredP
  pure $ PropTypeStatement (ArrayOf x) r

objectOfP :: PropTypeParser
objectOfP = do
  x <- ofSomething "objectOf" propTypeParser
  r <- requiredP
  pure $ PropTypeStatement (ObjectOf x) r

shapeP :: PropTypeParser
shapeP = do
  string "shape"
  xs <- parens $ objectOf propTypeParser
  r <- requiredP
  pure $ PropTypeStatement (Shape xs) r

exactP :: PropTypeParser
exactP = do
  string "exact"
  xs <- parens $ objectOf propTypeParser
  r <- requiredP
  pure $ PropTypeStatement (Exact xs) r

notSupportedP :: PropTypeParser
notSupportedP = pure $ PropTypeStatement NotSupported False

requiredP :: Parser Bool
requiredP = (/= Nothing) <$> (optional $ char '.' >> string "isRequired")

propTypeParser :: PropTypeParser
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
  pure propType

staticWordParser :: Parser ()
staticWordParser = do
  lexeme "static"
  lexeme "propTypes"
  symbol "="
  pure ()

staticPropParser :: Parser ()
staticPropParser = do
  some $ alphaNumChar
  char '.'
  lexeme "propTypes"
  symbol "="
  pure ()

propTypeStatementsParser :: Parser [(Text, PropTypeStatement)]
propTypeStatementsParser = do
  staticWordParser <|> staticPropParser
  xs <- objectOf propTypeParser
  optional $ char ';' 
  optional newline
  pure xs
