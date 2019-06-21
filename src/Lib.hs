module Lib
  ( startRefactor
  ) where

import qualified Turtle
import CollectPaths (findJsPaths, findTsPaths, makeAbsolute, pathsFromFile)
import ConvertImports (convert)
import RenameFile (rename)
import InplacePatterns (replaceJsExtensionInImports)
import Const

startRefactor :: IO ()
startRefactor = do
  paths <- makeAbsolute projectPath <$> pathsFromFile
  Turtle.cd projectPath
  -- jsPaths <- makeAbsolute projectPath <$> findJsPaths
  -- tsPaths <- makeAbsolute projectPath <$> findTsPaths
  traverse replaceJsExtensionInImports paths
  pure ()
