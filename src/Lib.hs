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
  , replaceExtendObservable
  , addTslintDisabled
  )
import PropTypes.Replace (replacePropTypes)
import Control.Monad (forM_)
import Data.Foldable (traverse_)

startRefactor :: IO ()
startRefactor = do
  currentPath <- getProjectPath
  tsPaths <- makeAbsolute currentPath <$> findTsPaths
  forM_ tsPaths convertToAbsolute
