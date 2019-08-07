{-# LANGUAGE OverloadedStrings #-}

module Parser.ExtendObservable where

import Parser.Common (Parser)
import Data.Text as T
import Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char

type ObservableDefinitions = M.Map T.Text T.Text

braces :: Parser T.Text
braces = do
  char '{'
  content <- many $ anySingleBut '}'
  char '}'
  pure $ "{" <> T.pack content <> "}"

classParser :: Parser T.Text
classParser = do
  skipManyTill anySingle $ string "class"
  skipMany $ anySingleBut '{'
  char '{'
  newline
  content <- many $ do
    a <- many $ anySingleBut '{'
    b <- braces
    pure $ (T.pack a) <> b
  char '}'
  skipMany anySingle
  pure $ "{" <> T.concat content <> "}"

extendObservableParser :: Parser ObservableDefinitions
extendObservableParser = do
  pure M.empty

addClassObsPropertiesParser :: T.Text -> Parser T.Text
addClassObsPropertiesParser classContent = do
  pure classContent
