{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ReplaceDefaultImports.ImportDefinition where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Common (Parser)

data ImportDefinition
  = Named
    { _name :: Text
    , _alias :: Maybe Text
    , _isDefault :: Bool
    }
  | Star { _alias :: Maybe Text }
  | Anonimous
  deriving (Eq, Show)

aliasParser :: Parser Text
aliasParser = do
  space1
  string "as"
  space1
  alias <- some letterChar
  pure $ pack alias

makeDefinition :: Text -> Maybe Text -> Bool -> ImportDefinition
makeDefinition name alias isDefault =
  if name == "*" then Star alias else Named name alias isDefault

aliasAndNameParser :: Parser (Text, Maybe Text)
aliasAndNameParser = do
  definitionName <- pack <$> (try (some letterChar) <|> some (char '*'))
  alias <- optional $ try aliasParser
  pure (definitionName, alias)

singleDefinitionParser :: Parser ImportDefinition
singleDefinitionParser = do
  space
  skipMany newline
  (name, alias) <- aliasAndNameParser
  optional $ char ','
  skipMany newline
  space
  pure $ makeDefinition name alias $ name == "default"

normalDefinitionsParser :: Parser [ImportDefinition]
normalDefinitionsParser = between (char '{') (char '}') (some singleDefinitionParser)

defaultDefinitionParser :: Parser [ImportDefinition]
defaultDefinitionParser = do
  (name, alias) <- aliasAndNameParser
  pure [makeDefinition name alias True]

manyImportDefinitionsParser :: Parser [ImportDefinition]
manyImportDefinitionsParser = do
  defaultDefinitions <- defaultDefinitionParser
  space
  char ','
  space
  normalDefinitions <- normalDefinitionsParser
  pure $ defaultDefinitions <> normalDefinitions

printAlias :: Maybe Text -> Text
printAlias (Just alias) = " as " <> alias
printAlias Nothing = ""

toJSImport :: ImportDefinition -> Text
toJSImport Named{..} = _name <> printAlias _alias
toJSImport Star{..} = "*" <> printAlias _alias
toJSImport Anonimous = ""

toJSImports :: [ImportDefinition] -> Text
toJSImports = foldr (\definition acc -> (toJSImport definition) <> ", " <> acc) ""
