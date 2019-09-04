{-# LANGUAGE OverloadedStrings #-}

module ReplaceDefaultImports.ExportDefinition where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)
import Common (Parser)
import ReplaceDefaultImports.Common (applicationParser, functionBodyParser, typeParser)

data ExportDefinition
  = Class (Maybe Text)
  | ObjectCreation Text
  | Lambda
  | Function Text
  | Const Text
  deriving (Eq, Show)

exportClassParser :: Parser ExportDefinition
exportClassParser = do
  string "class"
  space
  name <- optional $ try $ some alphaNumChar
  skipManyTill (try printChar <|> newline) (char '}')
  pure $ Class (pack <$> name)

exportObjectCreationParser :: Parser ExportDefinition
exportObjectCreationParser = do
  string "new"
  space
  name <- some alphaNumChar
  applicationParser
  optional . try $ char ';'
  pure $ ObjectCreation (pack name)

exportFunctionParser :: Parser ExportDefinition
exportFunctionParser = do
  string "function"
  space
  name <- some alphaNumChar
  space
  applicationParser
  skipMany typeParser
  functionBodyParser
  pure $ Function (pack name)

exportConstParser :: Parser ExportDefinition
exportConstParser = do
  string "const"
  space
  name <- some alphaNumChar
  skipManyTill printChar (char '=')
  skipManyTill (try printChar <|> newline) (char ';')
  pure $ Const (pack name)

exportLambdaParser :: Parser ExportDefinition
exportLambdaParser = do
  applicationParser
  skipMany typeParser
  string "=>"
  space
  try applicationParser <|> functionBodyParser
  optional . try $ char ';'
  pure Lambda

exportDefinitionParser :: Parser ExportDefinition
exportDefinitionParser =
  try exportClassParser
  <|> try exportObjectCreationParser
  <|> try exportFunctionParser
  <|> try exportConstParser
  <|> exportLambdaParser

toJSExport :: ExportDefinition -> Text -> Text
toJSExport (Class name) defaultName = "export class " <> (fromMaybe defaultName name)
