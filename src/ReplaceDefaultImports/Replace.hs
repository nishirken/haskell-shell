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
import System.FilePath.Posix (takeDirectory)
import ReplaceDefaultImports.Statement (Statement (..), statementParser)
import ReplaceDefaultImports.ImportDefinition (ImportDefinition (..))
import Data.List (find)
import Control.Monad (forM_)

newtype ImportStatements = ImportStatements [Statement] deriving (Eq, Show)
newtype ExportFromStatements = ExportFromStatements [Statement] deriving (Eq, Show)
newtype ExportStatements = ExportStatements [Statement] deriving (Eq, Show)

isScriptFile :: Statement -> Bool
isScriptFile statement =
  let
    extensions =
      [ ".scss"
      , ".css"
      , ".svg"
      , ".png"
      , ".jpg"
      ]
    f path = not . or $ map (\x -> Text.isSuffixOf x path) extensions in case statement of
      Import{..} -> f _path
      ExportFrom{..} -> f _path
      _ -> False

isProjectPath :: Statement -> Bool
isProjectPath statement = case statement of
  Import{..} -> any (`Text.isPrefixOf` _path)
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
  _ -> False

hasDefault :: Statement -> Bool
hasDefault statement = let f definitions = or [ _isDefault | Named{..} <- definitions ] in
  case statement of
    Import{..} -> f _definitions
    ExportFrom{..} -> f _definitions
    (Export _ isDefault) -> isDefault

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
    justPathName = Turtle.fromText $ Text.replace "./" "" (Text.pack $ Turtle.encodeString $ importPath)
    targetPath = currentPath </> justPathName
    indexPath = targetPath </> "index.ts"
    withExtension = do
      let
        tsPath = targetPath <.> "ts"
        tsxPath = targetPath <.> "tsx"
      isTsExists <- doesFileExist $ Turtle.encodeString tsPath
      pure $ if isTsExists then tsPath else tsxPath
  print targetPath
  isDirectory <- doesDirectoryExist $ Turtle.encodeString targetPath
  if isDirectory then pure indexPath else withExtension

lookupImport ::
  ImportDefinition
  -> FilePath
  -> FilePath
  -> M.Map FilePath ExportFromStatements
  -> M.Map FilePath ExportStatements
  -> IO Statement
lookupImport x@Named{..} importPath currentPath exportFromMap exportMap = do
  let err msg = error $ "Nothing founded in: " <> msg <> show x <> show currentPath
  targetPath <- resolveAndMakeAbsoluteImportPath currentPath importPath
  case M.lookup targetPath exportMap of
    (Just (ExportStatements exportStatements)) -> case Data.List.find (\(Export _ isDefault) -> isDefault) exportStatements of
        (Just x) -> pure x
        Nothing -> err "export statements"
    Nothing -> case M.lookup targetPath exportFromMap of
      (Just (ExportFromStatements exportFromStatements)) ->
        case Data.List.find (\Named{..} -> _isDefault) (concatMap (\ExportFrom{..} -> _definitions) exportFromStatements) of
        (Just x) -> lookupImport x importPath targetPath exportFromMap exportMap
        Nothing -> err "exportFromMap"
      Nothing -> err "exportMap"

replaceDefaultImports :: [FilePath] -> IO ()
replaceDefaultImports paths = do
  filesStatements <- traverse parseOne paths
  -- print filesStatements
  let
    defaultImportMap :: M.Map FilePath ImportStatements
    defaultImportMap = makeDefaultImportsMap filesStatements
    importAssocs :: [(FilePath, ImportStatements)]
    importAssocs = M.assocs defaultImportMap
    exportFromMap :: M.Map FilePath ExportFromStatements
    exportFromMap = makeExportFromMap filesStatements
    exportMap :: M.Map FilePath ExportStatements
    exportMap = makeExportMap filesStatements
  forM_
    importAssocs
    (\(path, (ImportStatements statements)) ->
      traverse
      (\Import{..} -> forM_ _definitions (\definition ->
        lookupImport definition (Turtle.fromText _path) path exportFromMap exportMap))
      statements)
  pure ()
