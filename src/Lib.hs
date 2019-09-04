module Lib
  ( startRefactor
  ) where

import qualified Turtle
import ProcessPaths
  ( findJsPaths
  , findTsPaths
  , makeAbsolute
  , pathsFromFile
  , intersectedPaths
  , getProjectPath
  )
import ConvertImports (convertToAbsolute)
import RenameFile (renameToTs)
import InplacePatterns (
  addComponentGenericsStub
  , replaceExportDefaultSingletons
  , replaceDefaultImports
  , replaceExtendObservable
  , addTslintDisabled
  )
import Control.Monad (forM_)
import Data.Foldable (traverse_)

startRefactor :: IO ()
startRefactor = do
  projectPath <- getProjectPath
  Turtle.cd projectPath
  tsPaths <- makeAbsolute projectPath <$> findTsPaths
  forM_ tsPaths convertToAbsolute
