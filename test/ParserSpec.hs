{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import Test.Hspec (describe, context, it, Spec, shouldBe)
import Text.Megaparsec (parse)
import Parser.ExportSingletons
import Parser.Statement
import Text.RawString.QQ (r)

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
    it "normal one line, one definition" $ do
      let
        testStr = "import { SomeClass } from './RelativeFolder/RelativeFile';"
        expect = Import [Definition "SomeClass" Nothing False] "./RelativeFolder/RelativeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "normal one line, multiple definitions" $ do
      let
        testStr = "import { StateClass, someConst } from 'SomeFolder/SomeFile';"
        expect = Import
          [ Definition "StateClass" Nothing False
          , Definition "someConst" Nothing False
          ]
          "SomeFolder/SomeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "one line * with alias" $ do
      let
        testStr = "import * as React from 'react';"
        expect = Import [Star (Just "React")] "react"
      parse statementParser "" testStr `shouldBe` Right expect
    it "normal one line, multiple definitions with alias" $ do
      let
        testStr = "import { StateClass, AnotherClass as Another, default as State } from 'folder/file';"
        expect = Import
          [ Definition "StateClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
      parse statementParser "" testStr `shouldBe` Right expect
    it "one line default without alias" $ do
      let
        testStr = "import SomeClass from 'SomeFolder/SomeFile';"
        expect = Import [Definition "SomeClass" Nothing True] "SomeFolder/SomeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "one line default with alias, the first" $ do
      let
        testStr = "import SomeClass as State from './RelativeFolder/RelativeFile';"
        expect = Import [Definition "SomeClass" (Just "State") True] "./RelativeFolder/RelativeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "one line default with alias, the second" $ do
      let
        testStr = "import { default as State } from './RelativeFolder/RelativeFile';"
        expect = Import [Definition "default" (Just "State") True] "./RelativeFolder/RelativeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "normal multiline without aliases" $ do
      let
        testStr = [r|import {
          SomeClass,
          someConst
        } from './SomeFolder/SomeFile';|]
        expect = Import
          [ Definition "SomeClass" Nothing False
          , Definition "someConst" Nothing False
          ]
          "./SomeFolder/SomeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "normal multiline with aliases" $ do
      let
        testStr = [r|import {
          StateClass,
          AnotherClass as Another,
          default as State,
        } from 'folder/file';|]
        expect = Import
          [ Definition "StateClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
      parse statementParser "" testStr `shouldBe` Right expect
    it "combination default and normal" $ do
      let
        testStr = [r|import State as AccessState, {
          SomeClass,
          AnotherClass as Another,
          default as State,
        } from 'folder/file';|]
        expect = Import
          [ Definition "State" (Just "AccessState") True
          , Definition "StateClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
      parse statementParser "" testStr `shouldBe` Right expect
  context "Export from" $ do
    it "normal one line, one definition" $ do
      let
        testStr = "export { SomeClass } from './RelativeFolder/RelativeFile';"
        expect = ExportFrom [Definition "SomeClass" Nothing False] "./RelativeFolder/RelativeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "normal one line, multiple definitions" $ do
      let
        testStr = "export { StateClass, someConst } from 'SomeFolder/SomeFile';"
        expect = ExportFrom
          [ Definition "StateClass" Nothing False
          , Definition "someConst" Nothing False
          ]
          "SomeFolder/SomeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "normal one line, multiple definitions with alias" $ do
      let
        testStr = "export { StateClass, AnotherClass as Another, default as State } from 'folder/file';"
        expect = ExportFrom
          [ Definition "StateClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
      parse statementParser "" testStr `shouldBe` Right expect
    it "one line * without alias" $ do
      let
        testStr = "export * from 'SomeFolder/SomeFile';"
        expect = ExportFrom [Star Nothing] "SomeFolder/SomeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "one line default with alias, the first" $ do
      let
        testStr = "export { default as State } from './RelativeFolder/RelativeFile';"
        expect = ExportFrom [Definition "default" (Just "State") True] "./RelativeFolder/RelativeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "normal multiline without aliases" $ do
      let
        testStr = [r|export {
          SomeClass,
          someConst
        } from './SomeFolder/SomeFile';|]
        expect = ExportFrom
          [ Definition "SomeClass" Nothing False
          , Definition "someConst" Nothing False
          ]
          "./SomeFolder/SomeFile"
      parse statementParser "" testStr `shouldBe` Right expect
    it "normal multiline with aliases" $ do
      let
        testStr = [r|export {
          SomeClass,
          AnotherClass as Another,
          default as State,
        } from 'folder/file';|]
        expect = ExportFrom
          [ Definition "StateClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
      parse statementParser "" testStr `shouldBe` Right expect
