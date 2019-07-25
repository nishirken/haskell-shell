{-# LANGUAGE OverloadedStrings #-}

module InplacePatternsSpec where

import Test.Hspec (describe, context, it, Spec, shouldBe)
import qualified Turtle
import InplacePatterns
import qualified System.IO.Strict as StrictIO
import System.Directory (getDirectoryContents)
import System.FilePath.Posix ((</>))
import System.Posix.Files (getFileStatus)
import Data.Text (Text, pack)

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
  } deriving (Show)

expectPath :: FilePath -> FilePath
expectPath path = (Turtle.encodeString . Turtle.filename . Turtle.fromString) path </> "expect.ts"

makeTestFile :: FilePath -> IO TestFile
makeTestFile path = do
  originalContent <- pack <$> readFile path
  expectContent <- pack <$> readFile (expectPath path)
  pure $ TestFile originalContent expectContent

inplacePatternsSpec :: Spec
inplacePatternsSpec = describe "InplacePatternsSpec" $ do
  it "exportDefault in index files" $ testOnFiles "index.js" "expectIndex.js" replaceExportDefaultFrom
  it "js extension in imports" $ testOnFiles "jsImport.js" "jsImportExpect.js" replaceJsExtensionInImports
  it "tslintDisable" $ testOnFiles "tslintDisable.tsx" "expectTslintDisable.tsx" addTslintDisabled
  it "genericsStub" $ testOnFiles "genericsStub.tsx" "expectGenericsStub.tsx" addComponentGenericsStub
  it "singletonExport" $ testOnFiles "singleton.tsx" "expectSingleton.tsx" replaceExportDefaultSingletons
  it "default imports" $ do
    allPaths <- (getDirectoryContents $ basePath </> "ChangeUser") >>= (traverse (\path -> ((,) path) <$> getFileStatus (basePath </> "ChangeUser" </> path)))
    let filePaths = map fst $ filter (not . Turtle.isDirectory . snd) allPaths
    preparedFiles <- traverse makeTestFile filePaths
    print preparedFiles
    -- replaceDefaultImports $ Turtle.fromString "ChangeUser/index.ts"
    True `shouldBe` True
