{-# LANGUAGE OverloadedStrings #-}

module Parser.ImportStatement where

import Parser.Common (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Parser.NormalImportStatement as Normal
import qualified Parser.DefaultImportStatement as Default
import Data.Text (pack)

defaultImportParser :: Parser Default.DefaultImportStatement
defaultImportParser = do
  string "import"
  space1
  definition <- some letterChar
  space1
  string "from"
  space1
  char '\''
  path <- some $ try letterChar <|> try (char '.') <|> char '/'
  string "';"
  pure $ Default.DefaultImportStatement (pack definition) (pack path)

normalImportParser :: Parser Normal.NormalImportStatement
normalImportParser = do
  string "import { "
  definitions <- some $ do
    definition <- some letterChar
    try (char ',' >> space1) <|> space1
    pure definition
  string "} from"
  space1
  char '\''
  path <- some $ try letterChar <|> try (char '.') <|> char '/'
  string "';"
  pure $ Normal.NormalImportStatement (map pack definitions) (pack path)
