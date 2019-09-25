{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module InplacePatterns where

import Turtle
import Prelude hiding (FilePath)
import ProcessPaths (isJsxFile, isIndexFile, getProjectPath)
import qualified Data.Text as Text
import qualified System.IO.Strict as StrictIO
import ExportSingletons (exportSingletonsParser)
import ExtendObservable (addClassObsPropertiesParser, classParser)
import Text.Megaparsec (parse, parseTest, ParseErrorBundle)
import Data.Either (fromRight, isRight)
import Data.Maybe (isJust, fromMaybe)
import Data.Void (Void)

textLinesFromFile :: FilePath -> IO [Text.Text]
textLinesFromFile path = do
  content <- StrictIO.readFile $ encodeString path
  pure $ (Text.lines . Text.pack) content

replace :: FilePath -> IO ()
replace = inplace (begins ("export default " *> pure "export "))

replaceExportDefaultFrom :: FilePath -> IO ()
replaceExportDefaultFrom path =
  if isIndexFile path then inplace ("export { default }" *> pure "export *") path else pure ()

replaceJsExtensionInImports :: FilePath -> IO ()
replaceJsExtensionInImports = inplace (".js';" *> pure "';")

addTslintDisabled :: FilePath -> IO ()
addTslintDisabled path = do
  content <- StrictIO.readFile $ encodeString path
  writeTextFile path (Text.pack $ "/* tslint:disable */\n" <> content)

addComponentGenericsStub :: FilePath -> IO ()
addComponentGenericsStub filePath = do
  lines <- textLinesFromFile filePath
  let replacedLines = (map (\line -> if (length (match (has "class" + suffix "Component {") line) == 2)
      then (Text.replace "Component {" "Component<any> {" line)
      else line) lines)
  writeTextFile filePath (Text.unlines replacedLines)

replaceExportDefaultSingletons :: FilePath -> IO ()
replaceExportDefaultSingletons path = do
  lines <- textLinesFromFile path
  let replacedLines = map (\line -> fromRight line $ parse exportSingletonsParser "" line) lines
  writeTextFile path (Text.unlines replacedLines)

parseClass :: String -> Text.Text -> Either (ParseErrorBundle Text Void) (Text.Text, Text.Text)
parseClass path content = do
  classRes <- parse classParser path content
  withDefinitions <- parse (addClassObsPropertiesParser classRes) path classRes
  pure (classRes, withDefinitions)

replaceExtendObservable :: FilePath -> IO ()
replaceExtendObservable path = do
  content <- Text.pack <$> StrictIO.readFile (encodeString path)
  case (parseClass (encodeString path) content) of
    (Right (oldRes, newRes)) -> writeFile (encodeString path) (Text.unpack $ Text.replace oldRes newRes content)
    (Left _) -> pure ()
