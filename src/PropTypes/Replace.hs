{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PropTypes.Replace (replacePropTypes) where

import qualified Turtle as T
import Data.Text (Text, pack, replace, unpack, lines, isPrefixOf, unlines, splitOn)
import PropTypes.ComponentStatement (ClassGenerics (..), ComponentStatement (..))
import PropTypes.ComponentParser (componentParser)
import PropTypes.Statement (PropTypeStatement (..), PropType (..))
import PropTypes.Parser (propTypeStatementsParser)
import PropTypes.ToTs (toTsInterface)
import qualified System.IO.Strict as StrictIO
import Text.Megaparsec (parse)
import Replace.Megaparsec (findAllCap)
import Data.Maybe (fromMaybe)
import Data.Either (isRight)
import Control.Monad (forM_)

-- need to choose priority if the component has props interface and propTypes at the same time
newPropsName :: Text -> Text -> Text
newPropsName componentName propsName = if propsName == "any" then componentName <> "Props" else propsName

newComponentLine :: (Text, ComponentStatement) -> Text 
newComponentLine (originalLine, Class{..}) = case _generics of
  (Just ClassGenerics{..}) -> replace _props (newPropsName _name _props) originalLine
  Nothing -> originalLine <> "<" <> (newPropsName _name "any") <> ">"
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

dropPropTypesImport :: Text -> Text
dropPropTypesImport = replace "import PropTypes from 'prop-types';\n" ""

insertPropsInterface :: Text -> [(Text, PropTypeStatement)] -> Text -> Text
insertPropsInterface className propTypes content =
  (Data.Text.unlines before)
  <> tsInterface
  <> (Data.Text.unlines after)
  where
    xs = Data.Text.lines content
    imports = filter (isPrefixOf "import") xs
    lastImport = if length imports == 0 then "" else last imports
    before = takeWhile (\x -> not $ isPrefixOf lastImport x) xs <> [lastImport]
    after = drop 1 $ dropWhile (\x -> not $ isPrefixOf lastImport x) xs
    tsInterface = "\n" <> toTsInterface (newPropsName className "any") propTypes <> "\n"

writeNewContent :: T.FilePath -> String -> IO ()
writeNewContent path oldContent = do
  let
    strPath = T.encodeString path
    textContent = pack oldContent
    parseWithCapture parser = parse (findAllCap parser) strPath textContent
    newContent = do
      componentLineResult <- filter isRight <$> parseWithCapture componentParser
      propTypesResult <- filter isRight <$> parseWithCapture propTypeStatementsParser
      let
        propTypes = [x | (Right x) <- propTypesResult] !! 0
        componentLine = [x | (Right x) <- componentLineResult] !! 0
        className Class{..} = _name
        className Functional{..} = _name
      pure
        $ dropPropTypes propTypes
        $ replaceOldLine componentLine
        $ dropPropTypesImport
        $ insertPropsInterface (className $ snd componentLine) (snd propTypes) textContent
  case newContent of
    (Left _) -> pure ()
    (Right xs) -> writeFile strPath (unpack xs)

replacePropTypes :: T.FilePath -> IO ()
replacePropTypes path = do
  content <- StrictIO.readFile $ T.encodeString path
  writeNewContent path content
