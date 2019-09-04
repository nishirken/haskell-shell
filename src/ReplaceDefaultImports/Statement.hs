{-# LANGUAGE OverloadedStrings #-}

module ReplaceDefaultImports.Statement where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Maybe (isJust)
import ReplaceDefaultImports.ImportDefinition
  ( ImportDefinition (..)
  , defaultDefinitionParser
  , normalDefinitionsParser
  , manyImportDefinitionsParser
  )
import ReplaceDefaultImports.ExportDefinition (ExportDefinition (..), exportDefinitionParser)
import ReplaceDefaultImports.Common
  ( applicationParser
  , functionBodyParser
  , typeParser
  )
import Common (Parser)

data Statement
  = Import
    { _definitions :: [ImportDefinition]
    , _path :: Text
    }
  | ExportFrom
    { _definitions :: [ImportDefinition]
    , _path :: Text
    }
  | Export ExportDefinition Bool
  deriving (Eq, Show)

pathParser :: Parser Text
pathParser = do
  space
  char '\''
  path <- some $ try alphaNumChar <|> try (char '.') <|> try (char '-') <|> char '/'
  string "';"
  pure $ pack path

baseStatementParser :: Parser ([ImportDefinition], Text)
baseStatementParser = do
  space
  definitions <- try manyImportDefinitionsParser <|> try normalDefinitionsParser <|> defaultDefinitionParser
  space
  string "from"
  path <- pathParser
  pure (definitions, path)

anonimousImportParser :: Parser Statement
anonimousImportParser = do
  skipMany $ try newline <|> spaceChar
  string "import"
  path <- pathParser
  pure $ Import [Anonimous] path

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
  statements <- some $ try importParser <|> try anonimousImportParser <|> try exportFromParser <|> try exportParser
  pure statements
