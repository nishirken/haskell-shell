{-# LANGUAGE OverloadedStrings #-}

module FileMatchers where

import Turtle
import Prelude hiding (FilePath)
import qualified Data.Text as Text

isIndexFile :: FilePath -> Bool
isIndexFile path = not . null . match (contains "index.") $ format fp path

isJsxFile :: FilePath -> IO Bool
isJsxFile path = not . null . match (contains "React") <$> readTextFile path

inPathsFile :: FilePath -> IO Bool
inPathsFile path =
  not . null . match (contains $ (text . Text.pack . encodeString) $ basename path)
  <$> readTextFile "paths.txt"
