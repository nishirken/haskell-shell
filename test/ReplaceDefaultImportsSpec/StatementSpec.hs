{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ReplaceDefaultImportsSpec.StatementSpec where

import Test.Hspec (describe, context, it, Spec, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)
import Data.Text (pack)
import ReplaceDefaultImports.ImportDefinition (ImportDefinition (..))
import ReplaceDefaultImports.ExportDefinition (ExportDefinition (..))
import ReplaceDefaultImports.Statement
  ( Statement (..)
  , importParser
  , exportFromParser
  , exportParser
  , statementParser
  )

statementSpec :: Spec
statementSpec = describe "Parser" $ do
  context "Import" $ do
    it "normal one line, one definition" $ do
      let
        testStr = "import { SomeClass } from './RelativeFolder/RelativeFile';"
        expect = Import [Named "SomeClass" Nothing False] "./RelativeFolder/RelativeFile"
      parse importParser "" testStr `shouldParse` expect
    it "normal one line, multiple definitions" $ do
      let
        testStr = "import { StateClass, someConst } from 'SomeFolder/SomeFile';"
        expect = Import
          [ Named "StateClass" Nothing False
          , Named "someConst" Nothing False
          ]
          "SomeFolder/SomeFile"
      parse importParser "" testStr `shouldParse` expect
    it "one line * with alias" $ do
      let
        testStr = "import * as React from 'react';"
        expect = Import [Star (Just "React")] "react"
      parse importParser "" testStr `shouldParse` expect
    it "normal one line, multiple definitions with alias" $ do
      let
        testStr = "import { StateClass, AnotherClass as Another, default as State } from 'folder/file';"
        expect = Import
          [ Named "StateClass" Nothing False
          , Named "AnotherClass" (Just "Another") False
          , Named "default" (Just "State") True
          ]
          "folder/file"
      parse importParser "" testStr `shouldParse` expect
    it "one line default without alias" $ do
      let
        testStr = "import SomeClass from 'SomeFolder/SomeFile';"
        expect = Import [Named "SomeClass" Nothing True] "SomeFolder/SomeFile"
      parse importParser "" testStr `shouldParse` expect
    it "one line default with alias, the first" $ do
      let
        testStr = "import SomeClass as State from './RelativeFolder/RelativeFile';"
        expect = Import [Named "SomeClass" (Just "State") True] "./RelativeFolder/RelativeFile"
      parse importParser "" testStr `shouldParse` expect
    it "one line default with alias, the second" $ do
      let
        testStr = "import { default as State } from './RelativeFolder/RelativeFile';"
        expect = Import [Named "default" (Just "State") True] "./RelativeFolder/RelativeFile"
      parse importParser "" testStr `shouldParse` expect
    it "normal multiline without aliases" $ do
      let
        testStr = [r|import {
          SomeClass,
          someConst
        } from './SomeFolder/SomeFile';|]
        expect = Import
          [ Named "SomeClass" Nothing False
          , Named "someConst" Nothing False
          ]
          "./SomeFolder/SomeFile"
      parse importParser "" testStr `shouldParse` expect
    it "normal multiline with aliases" $ do
      let
        testStr = [r|import {
          StateClass,
          AnotherClass as Another,
          default as State,
        } from 'folder/file';|]
        expect = Import
          [ Named "StateClass" Nothing False
          , Named "AnotherClass" (Just "Another") False
          , Named "default" (Just "State") True
          ]
          "folder/file"
      parse importParser "" testStr `shouldParse` expect
    it "combination default and normal" $ do
      let
        testStr = [r|import State as AccessState, {
          SomeClass,
          AnotherClass as Another,
          default as State,
        } from 'folder/file';|]
        expect = Import
          [ Named "State" (Just "AccessState") True
          , Named "SomeClass" Nothing False
          , Named "AnotherClass" (Just "Another") False
          , Named "default" (Just "State") True
          ]
          "folder/file"
      parse importParser "" testStr `shouldParse` expect
  context "Export from" $ do
    it "normal one line, one definition" $ do
      let
        testStr = "export { SomeClass } from './RelativeFolder/RelativeFile';"
        expect = ExportFrom [Named "SomeClass" Nothing False] "./RelativeFolder/RelativeFile"
      parse exportFromParser "" testStr `shouldParse` expect
    it "normal one line, multiple definitions" $ do
      let
        testStr = "export { StateClass, someConst } from 'SomeFolder/SomeFile';"
        expect = ExportFrom
          [ Named "StateClass" Nothing False
          , Named "someConst" Nothing False
          ]
          "SomeFolder/SomeFile"
      parse exportFromParser "" testStr `shouldParse` expect
    it "normal one line, multiple definitions with alias" $ do
      let
        testStr = "export { StateClass, AnotherClass as Another, default as State } from 'folder/file';"
        expect = ExportFrom
          [ Named "StateClass" Nothing False
          , Named "AnotherClass" (Just "Another") False
          , Named "default" (Just "State") True
          ]
          "folder/file"
      parse exportFromParser "" testStr `shouldParse` expect
    it "one line * without alias" $ do
      let
        testStr = "export * from 'SomeFolder/SomeFile';"
        expect = ExportFrom [Star Nothing] "SomeFolder/SomeFile"
      parse exportFromParser "" testStr `shouldParse` expect
    it "one line default with alias, the first" $ do
      let
        testStr = "export { default as State } from './RelativeFolder/RelativeFile';"
        expect = ExportFrom [Named "default" (Just "State") True] "./RelativeFolder/RelativeFile"
      parse exportFromParser "" testStr `shouldParse` expect
    it "normal multiline without aliases" $ do
      let
        testStr = [r|export {
          SomeClass,
          someConst
        } from './SomeFolder/SomeFile';|]
        expect = ExportFrom
          [ Named "SomeClass" Nothing False
          , Named "someConst" Nothing False
          ]
          "./SomeFolder/SomeFile"
      parse exportFromParser "" testStr `shouldParse` expect
    it "normal multiline with aliases" $ do
      let
        testStr = [r|export {
          SomeClass,
          AnotherClass as Another,
          default as State,
        } from 'folder/file';|]
        expect = ExportFrom
          [ Named "SomeClass" Nothing False
          , Named "AnotherClass" (Just "Another") False
          , Named "default" (Just "State") True
          ]
          "folder/file"
      parse exportFromParser "" testStr `shouldParse` expect
  context "Export" $ do
    it "export const" $ do
      let
        testStr = "export const Url = 'http://google.com';"
        expect = Export (Const "Url") False
      parse exportParser "" testStr `shouldParse` expect
    it "export class" $ do
      let
        testStr = [r|export class Url<T> {
          field1: string;
          field2: number;
        }|]
        expect = Export (Class (Just "Url")) False
      parse exportParser "" testStr `shouldParse` expect
    it "export function" $ do
      let
        testStr = [r|
        export function f(x: string): null {
          return null;
        }
        |]
        expect = Export (Function "f") False
      parse exportParser "" testStr `shouldParse` expect
    it "export default new class" $ do
      let
        testStr = "export default new SomeClass();"
        expect = Export (ObjectCreation "SomeClass") True
      parse exportParser "" testStr `shouldParse` expect
    it "export default class" $ do
      let
        testStr = "export default class SomeClass<T, S> {}"
        expect = Export (Class (Just "SomeClass")) True
      parse exportParser "" testStr `shouldParse` expect
    it "export default anonimous class" $ do
      let
        testStr = "export default class {}"
        expect = Export (Class Nothing) True
      parse exportParser "" testStr `shouldParse` expect
    it "export default lambda" $ do
      let
        testStr = [r|export default (): any => ({
          field: 1,
          field: 2,
        });|]
        expect = Export Lambda True
      parse exportParser "" testStr `shouldParse` expect

  context "All together" $ do
    let
      testStr = [r|
        import { StateClass, AnotherClass as Another, default as State } from 'folder/file';
        import State as AccessState, {
          SomeClass,
          AnotherClass as Another,
          default as State,
        } from 'folder/file';
        import Widget from 'widgets';

        import 'library';

        export const Url: string = 'http://localhost:3000';

        export default new SomeClass({ fields });

        export default new AnotherClass();

        export default class Component {

        }

        export default class {

        }

        class Class {}

        export default () => ({
          field: 1,
          field: 2,
        });

        function f() {
          
        }

        export function f(): null {
          return null;
        }

        export {
          SomeClass,
          AnotherClass as Another,
          default as State,
        } from 'folder/file';
        export * from 'SomeFolder/SomeFile';
        export { StateClass, AnotherClass as Another, default as State } from 'folder/file';
        |]
      expect =
        [ Import
          [ Named "StateClass" Nothing False
          , Named "AnotherClass" (Just "Another") False
          , Named "default" (Just "State") True
          ]
          "folder/file"
        , Import
          [ Named "State" (Just "AccessState") True
          , Named "SomeClass" Nothing False
          , Named "AnotherClass" (Just "Another") False
          , Named "default" (Just "State") True
          ]
          "folder/file"
        , Import [ Named "Widget" Nothing True ] "widgets"
        , Import [Anonimous] "library"
        , Export (Const "Url") False
        , Export (ObjectCreation "SomeClass") True
        , Export (ObjectCreation "AnotherClass") True
        , Export (Class (Just "Component")) True
        , Export (Class Nothing) True
        , Export Lambda True
        , Export (Function "f") False
        , ExportFrom
          [ Named "SomeClass" Nothing False
          , Named "AnotherClass" (Just "Another") False
          , Named "default" (Just "State") True
          ]
          "folder/file"
        , ExportFrom [Star Nothing] "SomeFolder/SomeFile"
        , ExportFrom
          [ Named "StateClass" Nothing False
          , Named "AnotherClass" (Just "Another") False
          , Named "default" (Just "State") True
          ]
          "folder/file"
        ]
    it "length" $ (length <$> parse statementParser "" testStr) `shouldBe` (Right $ length expect)
    it "compare parse" $ parse statementParser "" testStr `shouldParse` expect
  it "from file 1" $ do
    content <- pack <$> readFile "./test/testFiles/paths/component.tsx"
    let
      expect =
        [ Import [Star (Just "const")] "const"
        , Import [Anonimous] "services/validators"
        , Import [Named "SomeService" Nothing True] "services/SomeService"
        , Import
          [ Named "observable" Nothing False
          , Named "action" Nothing False
          ]
          "mobx"
        , Import
          [ Named "required" Nothing False
          , Named "length" Nothing False
          , Named "isPhone" Nothing False
          ]
          "services/validators"
        , Import [Named "formatPhone" Nothing False] "services/utils"
        , Export (Const "api") False
        , Export (Class (Just "State")) True
        ]
    parse statementParser "" content `shouldParse` expect
  it "from file 2" $ do
    content <- pack <$> readFile "./test/testFiles/ChangeUser/containers/ChangeUserState.ts"
    let
      expect =
        [ Import [Named "observable" Nothing False, Named "runInAction" Nothing False] "mobx"
        , Import [Named "result" Nothing False] "lodash" 
        , Export (ObjectCreation "ChangeUserState") True
        ]
    parse statementParser "" content `shouldParse` expect
