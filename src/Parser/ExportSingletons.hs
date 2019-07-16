{-# LANGUAGE OverloadedStrings #-}

module Parser.ExportSingletons where

import Parser.Common (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

type NewExportSingletonLine = T.Text

exportSingletonsParser :: Parser NewExportSingletonLine
exportSingletonsParser = do
  string "export default new "
  className <- some letterChar
  char '('
  classParams <- many $
    try letterChar
    <|> try (char '{')
    <|> try (char '}')
    <|> try (char '.')
    <|> try (char ',')
    <|> spaceChar
  char ')'
  char ';'
  let singletonName = toLower (head className) : tail className
  pure $ T.pack $ "export const " <> singletonName <> " = new " <> className <> "(" <> classParams <> ");"
