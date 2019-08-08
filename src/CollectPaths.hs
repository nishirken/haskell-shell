{-# LANGUAGE OverloadedStrings #-}

module CollectPaths where

import Turtle
import Prelude hiding (FilePath)
import qualified Data.Text as Text

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
