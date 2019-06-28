{-# LANGUAGE OverloadedStrings #-}

module InplacePatternsSpec where

import Test.Hspec (describe, context, it, Spec, shouldBe)
import qualified Turtle
import InplacePatterns
import qualified System.IO.Strict as StrictIO
import System.FilePath.Posix ((</>))

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

inplacePatternsSpec :: Spec
inplacePatternsSpec = describe "InplacePatternsSpec" $ do
  it "exportDefault in index files" $ testOnFiles "index.js" "expectIndex.js" replaceExportDefaultFrom
  it "js extension in imports" $ testOnFiles "jsImport.js" "jsImportExpect.js" replaceJsExtensionInImports
  it "tslintDisable" $ testOnFiles "tslintDisable.tsx" "expectTslintDisable.tsx" addTslintDisabled
