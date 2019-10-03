{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PropTypes.Replace (replacePropTypes, importParser) where

import qualified Turtle as T
import Data.Text (Text, pack, replace, unpack, lines, isPrefixOf, unlines, splitOn)
import PropTypes.ComponentStatement (ClassGenerics (..), ComponentStatement (..))
import PropTypes.ComponentParser (componentParser)
import PropTypes.Statement (PropTypeStatement (..), PropType (..))
import PropTypes.Parser (propTypeStatementsParser, lexeme, symbol)
import PropTypes.ToTs (toTsInterface)
import qualified System.IO.Strict as StrictIO
import Text.Megaparsec (parse)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import Replace.Megaparsec (findAllCap, findAll)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Either (isRight, rights)
import Control.Monad (forM_)
import Common (Parser)
import ReplaceDefaultImports.Statement (importParser)

newPropsName :: Text -> Text -> Text
newPropsName componentName propsName = if propsName == "any" then componentName <> "Props" else propsName

newComponentLine :: (Text, ComponentStatement) -> Text
newComponentLine (originalLine, Class{..}) = case _generics of
  (Just ClassGenerics{..}) ->
    let
      newName = newPropsName _name _props
      propsReplace = replace _props newName originalLine in
    case _state of
      (Just s) -> replace (newName <> ">") (s <> ">") propsReplace
      Nothing -> propsReplace
  Nothing -> originalLine <> "<" <> newPropsName _name "any" <> ">"
newComponentLine (originalLine, Functional{..}) = case _generic of
  (Just propsName) -> replace propsName (newPropsName _name propsName) originalLine
  Nothing -> "export const " <> _name <> ": React.FunctionalComponent<" <> (newPropsName _name "any") <> ">" <> rest
    where
      rest = last $ splitOn _name originalLine

replaceOldLine :: (Text, ComponentStatement) -> Text -> Text
replaceOldLine parserResult = replace oldLine newLine
  where
    oldLine = fst parserResult
    newLine = newComponentLine parserResult

dropPropTypes :: (Text, [(Text, PropTypeStatement)]) -> Text -> Text
dropPropTypes parserResult = replace (fst parserResult) ""
-- TODO context types???
dropPropTypesImport :: Text -> Text
dropPropTypesImport = replace "import PropTypes from 'prop-types';\n" ""

insertPropsInterface :: Text -> [Text] -> [(Text, PropTypeStatement)] -> Text -> Text
insertPropsInterface className imports propTypes content =
  before
  <> tsInterface
  <> after
  where
    lastImport = if length imports == 0 then "" else last imports
    splitted = if lastImport == "" then [content] else splitOn lastImport content
    before = head splitted <> lastImport <> "\n"
    after = last splitted
    tsInterface = "\n" <> toTsInterface (newPropsName className "any") propTypes

writeNewContent :: T.FilePath -> String -> IO ()
writeNewContent path oldContent = do
  let
    strPath = T.encodeString path
    textContent = pack oldContent
    parseWithCapture parser = parse (findAllCap parser) strPath textContent
    extractFirst = listToMaybe . rights
    newContent = do
      componentLine <- extractFirst <$> parseWithCapture componentParser
      propTypes <- extractFirst <$> parseWithCapture propTypeStatementsParser
      imports <- rights <$> parse (findAll importParser) strPath textContent
      let
        className Class{..} = _name
        className Functional{..} = _name
      case componentLine of
        (Just x) -> case propTypes of
          (Just y) -> pure
            $ dropPropTypes y
            $ replaceOldLine x
            $ dropPropTypesImport
            $ insertPropsInterface (className $ snd x) imports (snd y) textContent
          Nothing -> Right ""
        Nothing -> Right ""
  case newContent of
    (Left _) -> pure ()
    (Right xs) -> if xs == "" then pure () else writeFile strPath (unpack xs)

replacePropTypes :: T.FilePath -> IO ()
replacePropTypes path = do
  content <- StrictIO.readFile $ T.encodeString path
  writeNewContent path content
