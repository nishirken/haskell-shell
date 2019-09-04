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
import ReplaceDefaultImports.Statement (statementParser, Statement (..))
import ReplaceDefaultImports.ImportDefinition (ImportDefinition (..))
import ReplaceDefaultImports.ExportDefinition (ExportDefinition (..))
import Text.Megaparsec (parse, parseTest, ParseErrorBundle)
import Data.Either (fromRight, isRight)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Map.Strict as M
import Data.Void (Void)

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

printDefinition :: ImportDefinition -> Text.Text
printDefinition Named{..} = _name <> printAlias _alias
printDefinition Star{..} = "*" <> printAlias _alias
printDefinition Anonimous = ""

printDefinitions :: [ImportDefinition] -> Text.Text
printDefinitions = foldr (\definition acc -> (printDefinition definition) <> ", " <> acc) ""

printExportDefinition :: ExportDefinition -> Text.Text -> Text.Text
printExportDefinition (Class name) defaultName = "export class " <> (fromMaybe defaultName name)

printStatement :: Statement -> Text.Text
printStatement Import{..} = let isSingleDefault Named{..} = _isDefault in
  if length _definitions == 1 && isSingleDefault (head _definitions)
  then "import " <> printDefinition (head _definitions) <> " "
  else "import { " <> printDefinitions _definitions <> " } "
printStatement ExportFrom{..} = "export { " <> printDefinitions _definitions <> " } "
printStatement (Export definition isDefault) = printExportDefinition definition ""

newtype ImportStatements = ImportStatements [Statement] deriving (Eq, Show)
newtype ExportFromStatements = ExportFromStatements [Statement] deriving (Eq, Show)
newtype ExportStatements = ExportStatements [Statement] deriving (Eq, Show)

isScriptFile :: Statement -> Bool
isScriptFile Import{..} = not . or $ map (\x -> Text.isSuffixOf x _path)
  [ ".scss"
  , ".css"
  , ".svg"
  , ".png"
  , ".jpg"
  ]

isProjectPath :: Statement -> Bool
isProjectPath Import{..} = any (`Text.isPrefixOf` _path)
  [ "const"
  , "controls"
  , "hoc"
  , "layouts"
  , "routes"
  , "services"
  , "styles/variables"
  , "state"
  , "store"
  , "widgets"
  , "./"
  ]

hasDefault :: Statement -> Bool
hasDefault Import{..} = or [ _isDefault | Named{..} <- _definitions ]

makeDefaultImportsMap :: [(FilePath, [Statement])] -> M.Map FilePath ImportStatements
makeDefaultImportsMap xs = M.fromList $
  map (\(path, statements) -> (path, ImportStatements $
    filter (\x -> isScriptFile x && isProjectPath x && hasDefault x) [ x | x@Import{} <- statements ])) xs

makeExportFromMap :: [(FilePath, [Statement])] -> M.Map FilePath ExportFromStatements
makeExportFromMap xs = M.fromList $
  map (\(path, statements) -> (path, ExportFromStatements [ x | x@ExportFrom{} <- statements ])) xs

makeExportMap :: [(FilePath, [Statement])] -> M.Map FilePath ExportStatements
makeExportMap xs = M.fromList $
  map (\(path, statements) -> (path, ExportStatements [ x | x@Export{} <- statements ])) xs

parseOne :: FilePath -> IO (FilePath, [Statement])
parseOne path = do
  content <- Text.pack <$> StrictIO.readFile (encodeString path)
  pure $ (path, fromRight [] $ parse statementParser (encodeString path) content)

replaceDefaultImports :: [FilePath] -> IO ()
replaceDefaultImports paths = do
  projectPath <- getProjectPath
  parsed <- traverse parseOne paths
  let
    statements = map (\(path, x) -> (projectPath </> path, x)) parsed
    defaultImportMap = makeDefaultImportsMap statements
    exportFromMap = makeExportFromMap statements
    exportMap = makeExportMap statements
  print $ foldr (max . (\(ImportStatements xs) -> length xs) . snd) 0 $ M.toList defaultImportMap
  pure ()

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
