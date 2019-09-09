{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReplaceDefaultImportsSpec.ReplaceSpec where

import Test.Hspec (describe, it, Spec, shouldBe, runIO)
import ReplaceDefaultImports.Replace (replaceDefaultImports)
import System.FilePath.Posix ((</>), addExtension, dropExtension, takeDirectory, takeExtension, isExtensionOf)
import qualified System.IO.Strict as StrictIO
import qualified Turtle
import TestUtils
import qualified Data.Text as T
import Control.Monad (forM, forM_)
import System.Directory (getDirectoryContents)
import ProcessPaths (collectFilePaths)

initialFolder = basePath </> "ChangeUser"

data TestFile = TestFile
  { _originalContent :: T.Text
  , _expectContent :: T.Text
  , _path :: String
  } deriving (Show)

makeTestFile :: FilePath -> IO TestFile
makeTestFile originalPath = do
  originalContent <- T.pack <$> StrictIO.readFile originalPath
  let expectFileWithoutExt = dropExtension originalPath <> ".expect"
  expectContent <- T.pack <$> StrictIO.readFile
    (addExtension expectFileWithoutExt (takeExtension originalPath))
  pure $ TestFile originalContent expectContent originalPath

recoverFile :: TestFile -> IO ()
recoverFile TestFile{..} = writeFile _path $ T.unpack _originalContent

testOnFile :: TestFile -> Spec
testOnFile TestFile{..} =
  it _path $ _originalContent `shouldBe` _expectContent

replaceSpec :: Spec
replaceSpec = describe "Replace" $ do
  testPaths <- runIO (do
    let turtleShellPaths = Turtle.find (Turtle.has ".ts") $ Turtle.decodeString initialFolder
    testPaths <- map Turtle.encodeString <$> collectFilePaths turtleShellPaths
    let
      originalPaths :: [FilePath]
      originalPaths = filter
        (\path -> (isExtensionOf "ts" path || isExtensionOf "tsx" path) && (not . T.isSuffixOf ".expect" . T.pack . dropExtension) path) testPaths
    forM originalPaths makeTestFile)
  runIO $ replaceDefaultImports $ map (\TestFile{..} -> Turtle.decodeString _path) testPaths
  forM_ testPaths testOnFile
