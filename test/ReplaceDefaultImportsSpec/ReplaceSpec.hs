{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReplaceDefaultImportsSpec.ReplaceSpec where

import Test.Hspec (describe, it, Spec, shouldBe, runIO)
import ReplaceDefaultImports.Replace (replaceDefaultImports)
import System.FilePath.Posix ((</>), addExtension, dropExtension, takeDirectory, takeExtension, isExtensionOf)
import qualified System.IO.Strict as StrictIO
import TestUtils
import qualified Data.Text as T
import Control.Monad (forM)
import System.Directory (getDirectoryContents)

initialFolder = basePath </> "ChangeUser"

data TestFile = TestFile
  { _originalContent :: T.Text
  , _expectContent :: T.Text
  , _path :: String
  } deriving (Show)

makeTestFile :: FilePath -> IO TestFile
makeTestFile originalPath = do
  print $ "PATH: " <> originalPath
  originalContent <- T.pack <$> StrictIO.readFile originalPath
  expectContent <- T.pack <$> StrictIO.readFile (addExtension (takeExtension originalPath)
    $ dropExtension originalPath </> ".expect")
  pure $ TestFile originalContent expectContent originalPath

recoverFile :: TestFile -> IO ()
recoverFile TestFile{..} = writeFile _path $ T.unpack _originalContent

testOnFile :: TestFile -> IO ()
testOnFile TestFile{..} =
  _originalContent `shouldBe` _expectContent

replaceSpec :: Spec
replaceSpec = describe "Replace" $ do
  testPaths <- runIO (do
    testPaths <- getDirectoryContents initialFolder
    let
      originalPaths :: [FilePath]
      originalPaths = filter
        (\path -> (isExtensionOf "ts" path || isExtensionOf "tsx" path) && (not . T.isSuffixOf ".expect" . T.pack . dropExtension) path) testPaths
    print originalPaths
    forM originalPaths makeTestFile)
  it "index" $ do
    testOnFile $ testPaths !! 0
