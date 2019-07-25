{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module InplacePatterns where

import Turtle
import Prelude hiding (FilePath)
import FileMatchers (isJsxFile, isIndexFile)
import qualified Data.Text as Text
import qualified System.IO.Strict as StrictIO
import Parser.ExportSingletons (exportSingletonsParser)
import Parser.Statement (statementParser, Definition (..), ExportDefinition (..), Statement (..))
import Text.Megaparsec (parse, parseTest)
import Data.Either (fromRight)
import Data.Maybe (isJust)

textLinesFromFile :: FilePath -> IO [Text.Text]
textLinesFromFile path = do
  content <- StrictIO.readFile $ encodeString path
  pure $ (Text.lines . Text.pack) content

replace :: FilePath -> IO ()
replace path = do
  isJsx <- isJsxFile path
  pure ()
    where
      exportDefaultPattern = begins ("export default " *> pure "export ")

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

printAlias :: Maybe Text.Text -> Text.Text
printAlias (Just alias) = " as " <> alias
printAlias Nothing = ""

printDefinition :: Definition -> Text.Text
printDefinition Definition{..} = _name <> printAlias _alias
printDefinition Star{..} = "*" <> printAlias _alias
printDefinition Anonimous = ""

printDefinitions :: [Definition] -> Text.Text
printDefinitions = foldr (\definition acc -> (printDefinition definition) <> ", " <> acc) ""

getDefaultName :: 

printExportDefinition :: ExportDefinition -> Bool -> Text.Text
printExportDefinition (Class name) isDefault = 

printStatement :: Statement -> Text.Text
printStatement Import{..} = let isSingleDefault Definition{..} = _isDefault == True 
  if length _definitions == 1 && (isSingleDefault $ head _definitions) == True
  then "import " <> printDefinition (head _definitions) <> " "
  else "import { " <> printDefinitions _definitions <> " } "
printStatement ExportFrom{..} = "export { " <> printDefinitions _definitions <> " } "
printStatement (Export definition isDefault) = printExportDefinition definition isDefault

replaceDefaultImports :: FilePath -> IO ()
replaceDefaultImports path = do
  content <- Text.pack <$> StrictIO.readFile (encodeString path)
  let statements = parse statementParser (encodeString path) content
  print statements
  pure ()
