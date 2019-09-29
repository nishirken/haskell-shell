{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PropTypes.Replace (replacePropTypes) where

import qualified Turtle as T
import Data.Text (Text, pack, replace)
import PropTypes.ComponentStatement (ClassGenerics (..), ComponentStatement (..))
import PropTypes.ComponentParser (componentParser)
import PropTypes.Statement (PropTypeStatement (..), PropType (..))
import PropTypes.Parser (propTypeStatementsParser)
import qualified System.IO.Strict as StrictIO
import Text.Megaparsec (parse)
import Replace.Megaparsec (findAllCap)
import Data.Either (isRight)

-- need to choose priority if the component has props interface and propTypes at the same time
newPropsName :: Text -> Text -> Text
newPropsName componentName propsName = if propsName == "any" then componentName <> "Props" else propsName

newComponentLine :: (Text, ComponentStatement) -> Text 
newComponentLine (originalLine, Class{..}) = case _generics of
  (Just ClassGenerics{..}) -> replace _props (newPropsName _name _props) originalLine
  Nothing -> originalLine <> "<" <> _props <> ">"
newComponentLine (originalLine, Functional{..}) = case _generic of
  (Just x) -> replace _props (newPropsName _name _props) originalLine
  Nothing -> originalLine <> "<"

replacePropTypes :: T.FilePath -> IO ()
replacePropTypes path = do
  content <- StrictIO.readFile $ T.encodeString path
  let componentLines = parse (findAllCap componentParser) (T.encodeString path) (pack content)
  case componentLines of
    (Left _) -> pure ()
    (Right res) -> print $ filter isRight res
  pure ()
