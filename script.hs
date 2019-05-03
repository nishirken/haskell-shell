#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath)
import Data.Text (Text)

accFilePaths :: [FilePath] -> FilePath -> IO [FilePath]
accFilePaths acc path = pure $ path : acc

isIndexFile :: FilePath -> Bool
isIndexFile path = not . null $ match (contains "index.") $ format fp path

replaceInClassComponent :: FilePath -> IO ()
replaceInClassComponent path = do
  replace genericPattern
  replace tslintDisabledPattern
    where
      replace pattern = inplace pattern path
      genericPattern = ends ("Component {" *> pure "Component<any, any> {")
      tslintDisabledPattern = begins ("export " *> pure "/* tslint:disable */\nexport ")

rename :: FilePath -> IO ()
rename path = do
  mv path newPath
    where
      newPath :: FilePath
      newPath = (dropExtension path) <.> tsExt
      tsExt :: Text
      tsExt = if isIndexFile path then "ts" else "tsx"

main :: IO [()]
main = do
  paths <- foldShell (find (ends "js") ".") (FoldShell accFilePaths [] pure)
  traverse replaceInClassComponent $ filter (not . isIndexFile) paths
  traverse rename paths
