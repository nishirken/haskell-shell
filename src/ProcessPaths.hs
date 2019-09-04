{-# LANGUAGE OverloadedStrings #-}

module ProcessPaths where

import Turtle
import Prelude hiding (FilePath)
import qualified Data.Text as Text
import System.Directory (getHomeDirectory)

collectFilePaths :: Shell FilePath -> IO [FilePath]
collectFilePaths shellPaths = foldShell shellPaths (FoldShell accFilePaths [] pure)
  where
    accFilePaths acc path = pure $ path : acc

findJsPaths :: IO [FilePath]
findJsPaths = collectFilePaths $ find (ends ".js") "."

findTsPaths :: IO [FilePath]
findTsPaths = collectFilePaths $ find (ends ".ts" <|> ends ".tsx") "."

pathsFromFile :: IO [FilePath]
pathsFromFile = do
  content <- filter (/= "\n") . Text.lines <$> readTextFile "./paths.txt"
  pure $ map fromText content

makeAbsolute :: FilePath -> [FilePath] -> [FilePath]
makeAbsolute absPath = map (\path -> absPath </> fromText (Text.replace "./" "" ((Text.pack . encodeString) path)))

intersectedPaths :: [FilePath] -> [FilePath] -> [FilePath]
intersectedPaths originPaths =
  filter (\path -> dropExtension path `elem` withoutExtOrigin)
    where
      withoutExtOrigin :: [FilePath]
      withoutExtOrigin = map dropExtension originPaths

isIndexFile :: FilePath -> Bool
isIndexFile path = not . null . match (contains "index.") $ format fp path

isJsxFile :: FilePath -> IO Bool
isJsxFile path = not . null . match (contains "React") <$> readTextFile path

inPathsFile :: FilePath -> IO Bool
inPathsFile path =
  not . null . match (contains $ (text . Text.pack . encodeString) $ basename path)
  <$> readTextFile "paths.txt"

getProjectPath :: IO FilePath
getProjectPath = do
  homeDir <- getHomeDirectory
  pure $ decodeString homeDir </> "Projects/MonopolyOnline.Frontend/frontend/src/"
