{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TestUtils where

import qualified Turtle
import qualified System.IO.Strict as StrictIO
import Test.Hspec (shouldBe)
import System.FilePath.Posix ((</>), addExtension, dropExtension, takeDirectory)
import System.Posix.Files (getFileStatus)
import Data.Text (Text, pack, unpack, isSuffixOf)
import Data.Maybe (fromJust)
import Control.Monad (forM)

baseTestFilesPath :: FilePath
baseTestFilesPath = "./test/testFiles/"

testOnFiles :: FilePath -> FilePath -> (Turtle.FilePath -> IO ()) -> IO ()
testOnFiles testFile expectFile f = do
  let testPath = baseTestFilesPath </> testFile
  let expectPath = baseTestFilesPath </> expectFile
  initialContent <- StrictIO.readFile $ testPath
  expectContent <- StrictIO.readFile $ expectPath
  f $ Turtle.fromString testPath
  replacedContent <- StrictIO.readFile testPath
  writeFile testPath initialContent
  replacedContent `shouldBe` expectContent
