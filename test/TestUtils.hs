{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TestUtils where

import qualified Turtle
import qualified System.IO.Strict as StrictIO
import System.FilePath.Posix ((</>), addExtension, dropExtension, takeDirectory)
import System.Posix.Files (getFileStatus)
import System.Directory (getDirectoryContents)
import Data.Text (Text, pack, unpack, isSuffixOf)
import Data.Maybe (fromJust)
import Control.Monad (forM)

basePath :: FilePath
basePath = "./test/testFiles/"

testOnFiles :: FilePath -> FilePath -> (Turtle.FilePath -> IO ()) -> IO ()
testOnFiles testFile expectFile f = do
  let testPath = basePath </> testFile
  let expectPath = basePath </> expectFile
  initialContent <- StrictIO.readFile $ testPath
  expectContent <- StrictIO.readFile $ expectPath
  f $ Turtle.fromString testPath
  replacedContent <- StrictIO.readFile testPath
  writeFile testPath initialContent
  replacedContent `shouldBe` expectContent

data TestFile = TestFile
  { _originalContent :: Text
  , _expectContent :: Text
  , _path :: String
  } deriving (Show)

makeTestFile :: FilePath -> FilePath -> IO TestFile
makeTestFile path expectPath = do
  originalContent <- pack <$> StrictIO.readFile path
  expectContent <- pack <$> StrictIO.readFile expectPath
  pure $ TestFile originalContent expectContent path

recoverFile :: TestFile -> IO ()
recoverFile TestFile{..} = writeFile _path $ unpack _originalContent
-- TODO !!!!
testOnDirectory :: FilePath -> ([Turtle.FilePath] -> IO ()) -> IO ()
testOnDirectory initialFilePath f = do
  testPathsInDirectory <- takeDirectory initialFilePath >>= getDirectoryContents
  let paths = partition (\path -> not . isSuffixOf ".expect" . pack . dropExtension path) testFilesInDirectory
  testFiles <- forM paths (\(originPath, expectPath) -> makeTestFile originPath expectPath)
  f $ map (\TestFile{..} -> Turtle.encodeString _path)
  (map (\TestFile{..} -> _originalContent) testFiles) `shouldBe` (map (\TestFile{..} -> _expectContent) testFiles)
