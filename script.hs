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

replaceInClassComponent :: FilePath -> IO ()
replaceInClassComponent path = do
  inplace genericPattern path
  inplace tslintDisabledPattern path
    where
      genericPattern = ends ("Component {" *> pure "Component<any, any> {")
      tslintDisabledPattern = begins ("export " *> pure "/* tslint:disable */\nexport ")

replaceJsExport :: FilePath -> IO ()
replaceJsExport = inplace jsImportPattern
  where
    jsImportPattern = ends (".js';" *> pure "';")

rename :: FilePath -> IO ()
rename path = do
  mv path newPath
    where
      newPath :: FilePath
      newPath = (dropExtension path) <.> tsExt
      tsExt :: Text
      tsExt = if isIndexFile path then "ts" else "tsx"

findJsPaths :: IO [FilePath]
findJsPaths = collectFilePaths $ find (ends "js") "."

jsPathsFromFile :: IO [FilePath]
jsPathsFromFile = collectFilePaths $ (fromText . lineToText) <$> input "./paths.txt"

main :: IO [()]
main = do
  paths <- jsPathsFromFile
  
  traverse replaceInClassComponent $ filter (not . isIndexFile) paths
  traverse replaceJsExport paths
  traverse rename paths
