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
