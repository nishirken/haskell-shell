{-# LANGUAGE OverloadedStrings #-}

module ExtendObservable where

import Common (Parser)
import Data.Text as T
import Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type ObservableDefinitions = M.Map T.Text T.Text

braces :: Parser a -> Parser a
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
  value <- some $ try (char '[') <|> try (char ']') <|> try alphaNumChar <|> try (char '\'')
  optional $ char ','
  skipMany $ try newline <|> spaceChar
  pure (T.pack fieldName, T.pack value)

extendObservableParser :: Parser ObservableDefinitions
extendObservableParser = do
  skipManyTill anySingle $ string "extendObservable("
  skipMany $ anySingleBut '{'
  entries <- braces $ many . try $ entriesParser
  pure $ M.fromList entries

makeObservableDefinition :: (T.Text, T.Text) -> T.Text
makeObservableDefinition (name, value) =
  "\n  " <> "@observable public " <> name <> ": any = " <> value <> ";"

addClassObsPropertiesParser :: T.Text -> Parser T.Text
addClassObsPropertiesParser classContent = do
  definitionsMap <- extendObservableParser
  let definitions = (mconcat . Prelude.map makeObservableDefinition . M.toList) definitionsMap
  pure $ definitions <> classContent
