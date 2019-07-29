{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module InplacePatternsSpec where

import Test.Hspec (describe, context, it, Spec, shouldBe)
import qualified Turtle
import InplacePatterns
import qualified System.IO.Strict as StrictIO
import System.Directory (getDirectoryContents)
import System.FilePath.Posix ((</>), addExtension)
import System.Posix.Files (getFileStatus)
import Data.Text (Text, pack, unpack)
import Data.Maybe (fromJust)
import Control.Monad (forM_)

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

inplacePatternsSpec :: Spec
inplacePatternsSpec = describe "InplacePatterns" $ do
  it "exportDefault in index files" $ testOnFiles "index.js" "expectIndex.js" replaceExportDefaultFrom
  it "js extension in imports" $ testOnFiles "jsImport.js" "jsImportExpect.js" replaceJsExtensionInImports
  it "tslintDisable" $ testOnFiles "tslintDisable.tsx" "expectTslintDisable.tsx" addTslintDisabled
  it "genericsStub" $ testOnFiles "genericsStub.tsx" "expectGenericsStub.tsx" addComponentGenericsStub
  it "singletonExport" $ testOnFiles "singleton.tsx" "expectSingleton.tsx" replaceExportDefaultSingletons
  -- it "default imports" $ do
  --   let
  --     testFolder = basePath </> "ChangeUser"
  --     testFiles :: [FilePath]
  --     testFiles = map ((</>) testFolder)
  --       [ "index.ts"
  --       , "ChangeUser.ts"
  --       , "containers/ChangeUserContainer.ts"
  --       , "containers/ChangeUserState.ts"
  --       , "containers/index.ts"
  --       , "components/ChangeUserForm/ChangeUserForm.ts"
  --       , "components/ChangeUserForm/index.ts"
  --       ]
  --     expectFileName :: FilePath -> FilePath
  --     expectFileName path = let ext = (unpack . fromJust . Turtle.extension . Turtle.fromString) path in
  --       addExtension ((Turtle.encodeString . Turtle.dropExtension . Turtle.decodeString) path <> ".expect") ext
  --   preparedFiles <- traverse (\path -> makeTestFile path (expectFileName path)) testFiles
  --   let
  --     originalContent = map (\TestFile{..} -> _originalContent) preparedFiles
  --     expectContent = map (\TestFile{..} -> _expectContent) preparedFiles
  --   replaceDefaultImports (Turtle.fromString $ head testFiles)
  --   forM_ preparedFiles recoverFile
  --   originalContent `shouldBe` expectContent
