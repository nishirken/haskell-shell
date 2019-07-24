{-# LANGUAGE OverloadedStrings #-}

module Parser.Statement where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Parser.Common (Parser)
import Data.Maybe (isJust, fromMaybe)

data Definition =
  Definition
    { _name :: Text
    , _alias :: Maybe Text
    , _isDefault :: Bool
    }
  | Star { _alias :: Maybe Text }
  deriving (Eq, Show)

data ExportDefinition
  = Class (Maybe Text)
  | ObjectCreation Text
  | Lambda
  | Function Text
  | Const Text
  deriving (Eq, Show)

data Statement
  = Import
    { _definitions :: [Definition]
    , _path :: Text
    }
  | ExportFrom
    { _definitions :: [Definition]
    , _path :: Text
    }
  | Export ExportDefinition Bool
  deriving (Eq, Show)

aliasParser :: Parser Text
aliasParser = do
  space1
  string "as"
  space1
  alias <- some letterChar
  pure $ pack alias

aliasAndNameParser :: Parser (Text, Maybe Text)
aliasAndNameParser = do
  definitionName <- pack <$> (try (some letterChar) <|> some (char '*'))
  alias <- optional $ try aliasParser
  pure (definitionName, alias)

makeDefinition :: Text -> Maybe Text -> Bool -> Definition
makeDefinition name alias isDefault =
  if name == "*" then Star alias else Definition name alias isDefault

singleDefinitionParser :: Parser Definition
singleDefinitionParser = do
  space
  skipMany newline
  (name, alias) <- aliasAndNameParser
  optional $ char ','
  skipMany newline
  space
  pure $ makeDefinition name alias $ name == "default"

normalDefinitionsParser :: Parser [Definition]
normalDefinitionsParser = between (char '{') (char '}') (some singleDefinitionParser)

defaultDefinitionParser :: Parser [Definition]
defaultDefinitionParser = do
  (name, alias) <- aliasAndNameParser
  pure [makeDefinition name alias True]

pathParser :: Parser Text
pathParser = do
  space
  char '\''
  path <- some $ try letterChar <|> try (char '.') <|> char '/'
  string "';"
  pure $ pack path

combo :: Parser [Definition]
combo = do
  defaultDefinitions <- defaultDefinitionParser
  space
  char ','
  space
  normalDefinitions <- normalDefinitionsParser
  pure $ defaultDefinitions <> normalDefinitions

baseStatementParser :: Parser ([Definition], Text)
baseStatementParser = do
  space
  definitions <- try combo <|> try normalDefinitionsParser <|> defaultDefinitionParser
  space
  string "from"
  path <- pathParser
  pure (definitions, path)

importParser :: Parser Statement
importParser = do
  skipMany $ try newline <|> spaceChar
  string "import"
  (\(definitions, path) -> Import definitions path) <$> baseStatementParser

exportFromParser :: Parser Statement
exportFromParser = do
  skipMany $ try newline <|> spaceChar
  string "export"
  (\(definitions, path) -> ExportFrom definitions path) <$> baseStatementParser

exportClassParser :: Parser ExportDefinition
exportClassParser = do
  string "class"
  space
  name <- optional $ try $ some letterChar
  pure $ Class (pack <$> name)

exportObjectCreationParser :: Parser ExportDefinition
exportObjectCreationParser = do
  string "new"
  space
  name <- some letterChar
  char '('
  optional $ try (char '{') <|> char '['
  skipMany $ try spaceChar <|> try (char ',') <|> alphaNumChar
  optional $ try (char '}') <|> char ']'
  string ");"
  pure $ ObjectCreation (pack name)

exportFunctionParser :: Parser ExportDefinition
exportFunctionParser = do
  string "function"
  space
  name <- some letterChar
  pure $ Function (pack name)

exportConstParser :: Parser ExportDefinition
exportConstParser = do
  string "const"
  space
  name <- some letterChar
  pure $ Const (pack name)

exportLambdaParser :: Parser ExportDefinition
exportLambdaParser = do
  char '('
  optional $ try (char '{') <|> char '['
  skipMany $ try spaceChar <|> try (char ',') <|> alphaNumChar
  optional $ try (char '}') <|> char ']'
  char ')'
  space
  string "=>"
  pure Lambda

exportDefinitionParser :: Parser ExportDefinition
exportDefinitionParser =
  try exportClassParser
  <|> try exportObjectCreationParser
  <|> try exportFunctionParser
  <|> try exportConstParser
  <|> exportLambdaParser

exportParser :: Parser Statement
exportParser = do
  skipMany $ try newline <|> spaceChar
  string "export"
  space
  isDefault <- optional $ try (string "default")
  space
  definition <- exportDefinitionParser
  pure $ Export definition (isJust isDefault)

statementParser :: Parser [Statement]
statementParser = do
  imports <- some $ try importParser
  exports <- some $ try exportParser
  -- exportsFrom <- some $ try exportFromParser
  pure $ imports <> exports
