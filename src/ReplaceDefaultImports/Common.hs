{-# LANGUAGE OverloadedStrings #-}

module ReplaceDefaultImports.Common where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Common (Parser)

applicationParser :: Parser ()
applicationParser = do
  char '('
  skipManyTill (try printChar <|> newline) (char ')')
  pure ()

functionBodyParser :: Parser ()
functionBodyParser = do
  char '{'
  skipManyTill (try printChar <|> newline) (char '}')
  pure ()

typeParser :: Parser ()
typeParser = do
  try spaceChar <|> try (char ':') <|> try alphaNumChar <|> try (char '<') <|> try (char '>')
  pure ()
