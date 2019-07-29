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
  -- Turtle.cd curr
  -- condPaths <- traverse inPathsFile tsPaths
  -- let onlyChangedTsPaths = map fst $ filter (\(_, y) -> y == True) $ zip tsPaths condPaths
  replaceDefaultImports tsPaths
  pure ()
