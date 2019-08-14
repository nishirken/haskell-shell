module Lib
  ( startRefactor
  ) where

import qualified Turtle
import CollectPaths (findJsPaths, findTsPaths, makeAbsolute, pathsFromFile, intersectedPaths)
import ConvertImports (convert)
import RenameFile (rename)
import InplacePatterns (
  addComponentGenericsStub
  , replaceExportDefaultSingletons
  , replaceDefaultImports
  , replaceExtendObservable
  , addTslintDisabled
  )
import FileMatchers
import Const
import Control.Monad (forM_)
import Data.Foldable (traverse_)

startRefactor :: IO ()
startRefactor = do
  -- curr <- Turtle.pwd
  -- paths <- makeAbsolute projectPath <$> pathsFromFile
  Turtle.cd projectPath
  -- tsPaths <- makeAbsolute projectPath <$> findTsPaths
  jsPaths <- makeAbsolute projectPath <$> findJsPaths
  forM_ jsPaths rename 
