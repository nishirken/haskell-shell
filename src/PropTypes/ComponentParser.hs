{-# LANGUAGE OverloadedStrings #-}

module PropTypes.ComponentParser (componentParser) where

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
  skipMany $ alphaNumChar <|> char '.'
  generics <- optional $ between (symbol "<") (symbol ">") genericsParser
  pure $ Class (pack className) generics
-- TODO: handle all cases
functionalParser :: Parser ComponentStatement
functionalParser = do
  optional $ lexeme "export"
  lexeme "const"
  skipMany space1
  name <- some alphaNumChar
  skipMany space1
  symbol ":" <|> (symbol "=" >> lexeme "props =>")
  props <- optional $ do
    (lexeme "React.FunctionalComponent") <|> (lexeme "React.SFC") <|> (lexeme "FunctionalComponent") <|> (lexeme "SFC")
    between (symbol "<") (symbol ">") (some alphaNumChar)
  pure $ Functional (pack name) (pack <$> props)

componentParser :: Parser ComponentStatement
componentParser = classParser <|> functionalParser
