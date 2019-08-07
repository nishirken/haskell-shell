{-# LANGUAGE OverloadedStrings #-}

module Parser.ExtendObservable where

import Parser.Common (Parser)
import Data.Text as T
import Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type ObservableDefinitions = M.Map T.Text T.Text

braces :: Parser T.Text -> Parser T.Text
braces parser = do
  char '{'
  x <- parser
  char '}'
  pure x

betweenBraces :: Parser T.Text
betweenBraces = do
  a <- many anySingle
  b <- many . try $ braces betweenBraces
  c <- many anySingle
  pure $ (T.pack a) <> mconcat b <> (T.pack c)

classParser :: Parser T.Text
classParser = do
  skipManyTill anySingle $ string "class"
  skipManyTill anySingle $ string "{"
  content <- betweenBraces
  skipMany anySingle
  pure $ T.dropEnd 1 content

entriesParser :: Parser (T.Text, T.Text)
entriesParser = do
  skipMany $ try newline <|> spaceChar
  fieldName <- some alphaNumChar
  char ':'
  space
  value <- some $ try (char '[') <|> try (char ']') <|> alphaNumChar
  optional $ char ','
  pure (T.pack fieldName, T.pack value)

extendObservableParser :: Parser ObservableDefinitions
extendObservableParser = do
  skipManyTill anySingle $ string "extendObservable("
  skipManyTill anySingle $ string "{"
  entries <- many entriesParser
  skipManyTill anySingle $ string "});"
  pure $ M.fromList entries

addClassObsPropertiesParser :: T.Text -> Parser T.Text
addClassObsPropertiesParser classContent = do
  pure classContent
