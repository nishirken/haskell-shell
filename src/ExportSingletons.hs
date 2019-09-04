{-# LANGUAGE OverloadedStrings #-}

module ExportSingletons where

import Common (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Char (toLower)

exportSingletonsParser :: Parser T.Text
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
