{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Test.Hspec (describe, context, it, Spec, shouldBe)
import Text.Megaparsec (parse)
import Parser.ExportSingletons
import Parser.ImportStatement
import qualified Parser.DefaultImportStatement as Default
import qualified Parser.NormalImportStatement as Normal

parserSpec :: Spec
parserSpec = describe "Parser" $ do
  context "Export singleton" $ do
    it "first" $ do
      let
        testStr = "export default new ModalManager();"
        expect = "export const modalManager = new ModalManager();"
      parse exportSingletonsParser "" testStr `shouldBe` Right expect
    it "second" $ do
      let
        testStr = "export default new LoginFormState({ fields });"
        expect = "export const loginFormState = new LoginFormState({ fields });"
      parse exportSingletonsParser "" testStr `shouldBe` Right expect
  context "Import" $ do
    it "normal 1" $ do
      let
        testStr = "import { StateClass, someConst } from 'SomeFolder/SomeFile';"
        expect = Normal.NormalImportStatement ["StateClass", "someConst"] "SomeFolder/SomeFile"
      parse normalImportParser "" testStr `shouldBe` Right expect
    it "normal 2" $ do
      let
        testStr = "import { SomeClass } from './RelativeFolder/RelativeFile';"
        expect = Normal.NormalImportStatement ["SomeClass"] "./RelativeFolder/RelativeFile"
      parse normalImportParser "" testStr `shouldBe` Right expect
    it "default 1" $ do
      let
        testStr = "import SomeClass from 'SomeFolder/SomeFile';"
        expect = Default.DefaultImportStatement "SomeClass" "SomeFolder/SomeFile"
      parse defaultImportParser "" testStr `shouldBe` Right expect
    it "default 2" $ do
      let
        testStr = "import SomeClass from './RelativeFolder/RelativeFile';"
        expect = Default.DefaultImportStatement "SomeClass" "./RelativeFolder/RelativeFile"
      parse defaultImportParser "" testStr `shouldBe` Right expect
