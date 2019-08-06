{-# LANGUAGE OverloadedStrings #-}

module Parser.Extendobservable where

import Parser.Common (Parser)
import Data.Text as T
import Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char

type ObservableDefinitions = M.Map T.Text T.Text

classParser :: Parser T.Text
classParser = do
  skipMany newline
  skipManyTill printChar $ string "class"
  skipManyTill printChar $ char '{'
  content <- many (try printChar <|> newline)
  char '}'
  pure $ T.pack $ "{" <> content <> "}"

extendObservableParser :: Parser ObservableDefinitions
extendObservableParser = do
  pure M.empty

addClassObsPropertiesParser :: T.Text -> Parser T.Text
addClassObsPropertiesParser classContent = do
  pure classContent
