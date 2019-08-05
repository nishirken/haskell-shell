module Lib
  ( startRefactor
  ) where

import qualified Turtle
import CollectPaths (findJsPaths, findTsPaths, makeAbsolute, pathsFromFile)
import ConvertImports (convert)
import RenameFile (rename)
import InplacePatterns (addComponentGenericsStub, replaceExportDefaultSingletons, replaceDefaultImports)
import FileMatchers
import Const

startRefactor :: IO ()
startRefactor = do
  -- curr <- Turtle.pwd
  -- paths <- makeAbsolute projectPath <$> pathsFromFile
  Turtle.cd projectPath
  -- jsPaths <- makeAbsolute projectPath <$> findJsPaths
  tsPaths <- makeAbsolute projectPath <$> findTsPaths
  replaceDefaultImports tsPaths
  pure ()
