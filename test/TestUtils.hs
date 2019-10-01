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
import Control.Exception

baseTestFilesPath :: FilePath
baseTestFilesPath = "./test/testFiles/"

testOnFiles :: FilePath -> FilePath -> (Turtle.FilePath -> IO ()) -> IO ()
testOnFiles testFile expectFile f = do
  let testPath = baseTestFilesPath </> testFile
  let expectPath = baseTestFilesPath </> expectFile
  initialContent <- StrictIO.readFile $ testPath
  expectContent <- StrictIO.readFile $ expectPath
  result <- try $ f $ Turtle.fromString testPath :: IO (Either ErrorCall ())
  replacedContent <- StrictIO.readFile testPath
  case result of
    (Left e) -> do
      print e
      writeFile testPath initialContent
      replacedContent `shouldBe` expectContent
    (Right _) -> do
      writeFile testPath initialContent
      replacedContent `shouldBe` expectContent
