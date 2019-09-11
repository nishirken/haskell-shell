{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReplaceDefaultImports.Replace where

import Turtle
import Prelude hiding (FilePath)
import Text.Megaparsec (parse)
import ProcessPaths (getProjectPath)
import qualified Data.Text as Text
import qualified Data.Map.Strict as M
import qualified System.IO.Strict as StrictIO
import Data.Either (fromRight)
import System.Directory (doesDirectoryExist, doesFileExist)
import ReplaceDefaultImports.Statement (Statement (..), statementParser)
import ReplaceDefaultImports.ImportDefinition (ImportDefinition (..))

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

resolveAndMakeAbsoluteImportPath :: FilePath -> FilePath -> IO FilePath
resolveAndMakeAbsoluteImportPath currentPath importPath = do
  let
    isAbsolute = not $ Text.isPrefixOf "./" (Text.pack $ Turtle.encodeString importPath)
    targetPath = currentPath </>
      if isAbsolute then importPath else (Turtle.fromText $ Text.replace "./" "" (Text.pack $ Turtle.encodeString importPath))
    withExtension = do
      let
        tsPath = targetPath <.> "ts"
        tsxPath = targetPath <.> "tsx"
      isTsExists <- doesFileExist $ Turtle.encodeString tsPath
      pure $ if isTsExists then tsPath else tsxPath
    indexPath = targetPath </> "index.ts"
  isDirectory <- doesDirectoryExist $ Turtle.encodeString targetPath
  print (isDirectory, targetPath)
  if isDirectory then pure indexPath else withExtension

replaceDefaultImports :: [FilePath] -> IO ()
replaceDefaultImports paths = do
  filesStatements <- traverse parseOne paths
  let
    defaultImportMap :: M.Map FilePath ImportStatements
    defaultImportMap = makeDefaultImportsMap filesStatements
    exportFromMap :: M.Map FilePath ExportFromStatements
    exportFromMap = makeExportFromMap filesStatements
    exportMap :: M.Map FilePath ExportStatements
    exportMap = makeExportMap filesStatements
  pure ()
