{-# LANGUAGE OverloadedStrings #-}

module ReplaceDefaultImportsSpec.ResolveImportsSpec where

import Test.Hspec (describe, it, shouldBe, Spec, runIO)
import TestUtils (baseTestFilesPath)
import ReplaceDefaultImports.Replace (resolveAndMakeAbsoluteImportPath)
import qualified Turtle
import ProcessPaths (getProjectPath)

resolveImportsSpec :: Spec
resolveImportsSpec = describe "ResolveImports" $ do
  projectPath <- runIO (do
    c <- Turtle.pwd
    pure c)
  runIO $ Turtle.cd $ Turtle.decodeString baseTestFilesPath

  it "two paths are absolute" $ do
    path <- resolveAndMakeAbsoluteImportPath "rootFolder/someFile" "rootFolder/anotherFolder/someFile"
    path `shouldBe` "rootFolder/anotherFolder/someFile"

  it "currentPath is absolute and importPath is relative" $ do
    path <- resolveAndMakeAbsoluteImportPath "rootFolder/someFile" ""
    path `shouldBe` ""

  runIO $ Turtle.cd projectPath
