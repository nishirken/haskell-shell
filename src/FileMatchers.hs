{-# LANGUAGE OverloadedStrings #-}

module FileMatchers where

import Turtle
import Prelude hiding (FilePath)

isIndexFile :: FilePath -> Bool
isIndexFile path = not . null . match (contains "index.") $ format fp path

isJsxFile :: FilePath -> IO Bool
isJsxFile path = not . null . match (contains "React") <$> readTextFile path
