{-# LANGUAGE OverloadedStrings #-}

module Parser.Statement where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Parser.Common (Parser)

data Definition =
  Definition
    { _name :: Text
    , _alias :: Maybe Text
    , _isDefault :: Bool
    }
  | Star { _alias :: Maybe Text }
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
  string "import"
  (\(definitions, path) -> Import definitions path) <$> baseStatementParser

exportFromParser :: Parser Statement
exportFromParser = do
  string "export"
  (\(definitions, path) -> ExportFrom definitions path) <$> baseStatementParser

statementParser :: Parser Statement
statementParser = try importParser <|> exportFromParser
