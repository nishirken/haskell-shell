#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath)
import Data.Text (Text)

collectFilePaths :: Shell FilePath -> IO [FilePath]
collectFilePaths shellPaths = foldShell shellPaths (FoldShell accFilePaths [] pure)
  where
    accFilePaths acc path = pure $ path : acc

isIndexFile :: FilePath -> Bool
isIndexFile path = not . null $ match (contains "index.") $ format fp path

isJsxFile :: FilePath -> IO Bool
isJsxFile path = not . null . (match (contains "React")) <$> readTextFile path

replace :: FilePath -> IO ()
replace path = do
  isJsx <- isJsxFile path
  inplace (firstRoundPattern isJsx) path
  inplace secondRoundPattern path
    where
      firstRoundPattern isJsx = if isIndexFile path
        then exportDefaultFromPattern
        else if isJsx then jsxFilePatterns else tsPatterns
      secondRoundPattern = jsImportPattern <|> exportDefaultPattern <|> propTypesPattern
      tsPatterns = tslintDisabledPattern <|> jsImportPattern
      jsxFilePatterns = tsPatterns <|> genericPattern
      genericPattern = "Component {" *> pure "Component<any, any> {"
      exportDefaultPattern = "export default " *> pure "export "
      tslintDisabledPattern = "export " *> pure "/* tslint:disable */\nexport "
      jsImportPattern = ends (".js';" *> pure "';")
      exportDefaultFromPattern = begins ("export { default }" *> pure "export *")
      -- TODO: propTypes pattern
      propTypesPattern =
        (between "static propTypes = {" "};" (plus anyChar)) *> pure ""

rename :: FilePath -> IO ()
rename path = do
  isJsx <- isJsxFile path
  mv path $ newPath isJsx
    where
      newPath ::Bool -> FilePath
      newPath isJsx = dropExtension path <.> tsExt isJsx
      tsExt :: Bool -> Text
      tsExt isJsx = if isJsx then "tsx" else "ts"

findJsPaths :: IO [FilePath]
findJsPaths = collectFilePaths $ find (ends "js") "."

jsPathsFromFile :: IO [FilePath]
jsPathsFromFile = collectFilePaths $ (fromText . lineToText) <$> input "./paths.txt"

main :: IO [()]
main = do
  cd "Project"
  paths <- findJsPaths
  -- paths <- jsPathsFromFile
  
  traverse replace paths
  traverse rename paths
