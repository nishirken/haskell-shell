{-# LANGUAGE OverloadedStrings #-}

module ReplaceDefaultImportsSpec.ResolveImportsSpec where

import Test.Hspec (describe, it, shouldBe, Spec, runIO, context)
import TestUtils (baseTestFilesPath)
import ReplaceDefaultImports.Replace (resolveAndMakeAbsoluteImportPath)
import qualified Turtle
import System.FilePath.Posix ((</>))
import ProcessPaths (getProjectPath)
import ReplaceDefaultImportsSpec.ReplaceSpec (initialFolder)

expectPath :: FilePath -> Turtle.FilePath
expectPath path = Turtle.decodeString $ initialFolder </> path

turtleInitialFolder = Turtle.decodeString initialFolder

resolveImportsSpec :: Spec
resolveImportsSpec = describe "ResolveImports" $ do
  projectPath <- runIO $ Turtle.pwd
  runIO $ Turtle.cd $ Turtle.decodeString baseTestFilesPath

  context "two paths are absolute" $ do
    it "containers index" $ do
      path <- resolveAndMakeAbsoluteImportPath turtleInitialFolder "containers"
      path `shouldBe` expectPath "containers/index.ts"

    it "containers ChangeUserState" $ do
      path <- resolveAndMakeAbsoluteImportPath turtleInitialFolder "containers/ChangeUserState"
      path `shouldBe` expectPath "containers/ChangeUserState.ts"
    
    it "containers ChangeUserContainer" $ do
      path <- resolveAndMakeAbsoluteImportPath turtleInitialFolder "containers/ChangeUserContainer"
      path `shouldBe` expectPath "containers/ChangeUserContainer.ts"

    it "components ChangeUserForm" $ do
      path <- resolveAndMakeAbsoluteImportPath turtleInitialFolder "components/ChangeUserForm/ChangeUserForm"
      path `shouldBe` expectPath "components/ChangeUserForm/ChangeUserForm.tsx"

    it "components index" $ do
      path <- resolveAndMakeAbsoluteImportPath turtleInitialFolder "components/ChangeUserForm"
      path `shouldBe` expectPath "components/ChangeUserForm/index.ts"

  context "currentPath is absolute and importPath is relative" $ do
    it "containers ChangeUserContainer" $ do
      path <- resolveAndMakeAbsoluteImportPath (turtleInitialFolder Turtle.</> "containers") "./ChangeUserContainer"
      path `shouldBe` expectPath "containers/ChangeUserContainer.ts"

    it "components ChangeUserForm" $ do
      path <- resolveAndMakeAbsoluteImportPath (turtleInitialFolder Turtle.</> "components") "./ChangeUserForm/ChangeUserForm"
      path `shouldBe` expectPath"components/ChangeUserForm.tsx"

  runIO $ Turtle.cd projectPath
