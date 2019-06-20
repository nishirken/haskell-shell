module Lib
  ( startRefactor
  ) where

import qualified Turtle
import CollectPaths (findJsPaths, findTsPaths, makeAbsolute)
import ConvertImports (convert)
import Const

startRefactor :: IO ()
startRefactor = do
  Turtle.cd projectPath
  paths <- makeAbsolute projectPath <$> findJsPaths
  tsPaths <- makeAbsolute projectPath <$> findTsPaths
  -- paths <- jsPathsFromFile
  -- traverse replace paths
  -- traverse replace tsPaths
  -- traverse rename paths
--  traverse convert tsPaths
  traverse convert (paths <> tsPaths)
  pure ()
