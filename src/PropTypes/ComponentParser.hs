{-# LANGUAGE OverloadedStrings #-}

module PropTypes.ComponentParser where

import PropTypes.ComponentStatement (ClassGenerics (..), ComponentStatement (..))
import Common (Parser)
import Data.Text (pack)
import PropTypes.Parser (lexeme, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char

genericsParser :: Parser ClassGenerics
genericsParser = do
  props <- lexeme $ some alphaNumChar
  state <- optional $ do
    symbol ","
    lexeme $ some alphaNumChar
  pure $ ClassGenerics (pack props) (pack <$> state)

classParser :: Parser ComponentStatement
classParser = do
  className <- between (symbol "class") (symbol "extends") (lexeme $ do
    xs <- some alphaNumChar
    notFollowedBy alphaNumChar
    pure xs)
  generics <- optional $ between (symbol "<") (symbol ">") genericsParser
  pure $ Class (pack className) generics

componentParser :: Parser ComponentStatement
componentParser = classParser
