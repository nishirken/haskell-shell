{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import Test.Hspec (describe, context, it, Spec, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)
import Parser.ExportSingletons
import Parser.Statement
import Parser.ExtendObservable
import Text.RawString.QQ (r)
import Data.Text (pack)
import qualified Data.Map as M

parserSpec :: Spec
parserSpec = describe "Parser" $ do
  context "Export singleton" $ do
    it "first" $ do
      let
        testStr = "export default new Manager();"
        expect = "export const manager = new Manager();"
      parse exportSingletonsParser "" testStr `shouldParse` expect
    it "second" $ do
      let
        testStr = "export default new State({ fields });"
        expect = "export const state = new State({ fields });"
      parse exportSingletonsParser "" testStr `shouldParse` expect
  context "Import" $ do
    it "normal one line, one definition" $ do
      let
        testStr = "import { SomeClass } from './RelativeFolder/RelativeFile';"
        expect = Import [Definition "SomeClass" Nothing False] "./RelativeFolder/RelativeFile"
      parse importParser "" testStr `shouldParse` expect
    it "normal one line, multiple definitions" $ do
      let
        testStr = "import { StateClass, someConst } from 'SomeFolder/SomeFile';"
        expect = Import
          [ Definition "StateClass" Nothing False
          , Definition "someConst" Nothing False
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
          [ Definition "StateClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
      parse importParser "" testStr `shouldParse` expect
    it "one line default without alias" $ do
      let
        testStr = "import SomeClass from 'SomeFolder/SomeFile';"
        expect = Import [Definition "SomeClass" Nothing True] "SomeFolder/SomeFile"
      parse importParser "" testStr `shouldParse` expect
    it "one line default with alias, the first" $ do
      let
        testStr = "import SomeClass as State from './RelativeFolder/RelativeFile';"
        expect = Import [Definition "SomeClass" (Just "State") True] "./RelativeFolder/RelativeFile"
      parse importParser "" testStr `shouldParse` expect
    it "one line default with alias, the second" $ do
      let
        testStr = "import { default as State } from './RelativeFolder/RelativeFile';"
        expect = Import [Definition "default" (Just "State") True] "./RelativeFolder/RelativeFile"
      parse importParser "" testStr `shouldParse` expect
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
      parse importParser "" testStr `shouldParse` expect
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
      parse importParser "" testStr `shouldParse` expect
    it "combination default and normal" $ do
      let
        testStr = [r|import State as AccessState, {
          SomeClass,
          AnotherClass as Another,
          default as State,
        } from 'folder/file';|]
        expect = Import
          [ Definition "State" (Just "AccessState") True
          , Definition "SomeClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
      parse importParser "" testStr `shouldParse` expect
  context "Export from" $ do
    it "normal one line, one definition" $ do
      let
        testStr = "export { SomeClass } from './RelativeFolder/RelativeFile';"
        expect = ExportFrom [Definition "SomeClass" Nothing False] "./RelativeFolder/RelativeFile"
      parse exportFromParser "" testStr `shouldParse` expect
    it "normal one line, multiple definitions" $ do
      let
        testStr = "export { StateClass, someConst } from 'SomeFolder/SomeFile';"
        expect = ExportFrom
          [ Definition "StateClass" Nothing False
          , Definition "someConst" Nothing False
          ]
          "SomeFolder/SomeFile"
      parse exportFromParser "" testStr `shouldParse` expect
    it "normal one line, multiple definitions with alias" $ do
      let
        testStr = "export { StateClass, AnotherClass as Another, default as State } from 'folder/file';"
        expect = ExportFrom
          [ Definition "StateClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
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
        expect = ExportFrom [Definition "default" (Just "State") True] "./RelativeFolder/RelativeFile"
      parse exportFromParser "" testStr `shouldParse` expect
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
      parse exportFromParser "" testStr `shouldParse` expect
    it "normal multiline with aliases" $ do
      let
        testStr = [r|export {
          SomeClass,
          AnotherClass as Another,
          default as State,
        } from 'folder/file';|]
        expect = ExportFrom
          [ Definition "SomeClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
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

  context "context" $ do
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

        export default class Component {

        }

        export default class {

        }

        export default () => ({
          field: 1,
          field: 2,
        });

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
          [ Definition "StateClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
        , Import
          [ Definition "State" (Just "AccessState") True
          , Definition "SomeClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
        , Import [ Definition "Widget" Nothing True ] "widgets"
        , Import [Anonimous] "library"
        , Export (Const "Url") False
        , Export (ObjectCreation "SomeClass") True
        , Export (Class (Just "Component")) True
        , Export (Class Nothing) True
        , Export Lambda True
        , Export (Function "f") False
        , ExportFrom
          [ Definition "SomeClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
        , ExportFrom [Star Nothing] "SomeFolder/SomeFile"
        , ExportFrom
          [ Definition "StateClass" Nothing False
          , Definition "AnotherClass" (Just "Another") False
          , Definition "default" (Just "State") True
          ]
          "folder/file"
        ]
    it "length" $ (length <$> parse statementParser "" testStr) `shouldBe` (Right $ length expect)
    it "compare parse" $ parse statementParser "" testStr `shouldParse` expect
  it "from file" $ do
    content <- pack <$> readFile "./test/testFiles/paths/component.tsx"
    let
      expect =
        [ Import [Star (Just "const")] "const"
        , Import [Anonimous] "services/validators"
        , Import [Definition "SomeService" Nothing True] "services/SomeService"
        , Import
          [ Definition "observable" Nothing False
          , Definition "action" Nothing False
          ]
          "mobx"
        , Import
          [ Definition "required" Nothing False
          , Definition "length" Nothing False
          , Definition "isPhone" Nothing False
          ]
          "services/validators"
        , Import [Definition "formatPhone" Nothing False] "services/utils"
        , Export (Const "api") False
        , Export (Class (Just "State")) True
        ]
    parse statementParser "" content `shouldParse` expect

  context "ExtendObservable" $ do
    it "classParser" $ do
      let
        testStr = [r|
          import { action, extendObservable } from 'mobx';

          export default abstract class State {
            loading: any;
            otherField: any;

            constructor() {
              this.reset();
            }

            @action
            protected reset(): void {
              extendObservable(this, {
                loading: false,
              });
            }
          }|]
        expect = [r|
            loading: any;
            otherField: any;

            constructor() {
              this.reset();
            }

            @action
            protected reset(): void {
              extendObservable(this, {
                loading: false,
              });
            }
          |]
      parse classParser "" testStr `shouldParse` expect

    it "extendObservableParser" $ do
      let
        testStr = [r|
          extendObservable(this, {
            loading: false,
            list: [],
            str: '',
          });
        |]
        expect = M.fromList [("loading", "false"), ("list", "[]"), ("str", "''")]
      parse extendObservableParser "" testStr `shouldParse` expect
    it "addClassObsPropertiesParser" $ do
      let
        testStr = [r|
          import { action, extendObservable } from 'mobx';

          export default abstract class State {
            loading: any;
            otherField: any;

            constructor() {
              this.reset();
            }

            @action
            protected reset(): void {
              extendObservable(this, {
                loading: false,
                list: [],
                str: '',
              });
            }
          }|]
        classContent = [r|
            loading: any;
            otherField: any;

            constructor() {
              this.reset();
            }

            @action
            protected reset(): void {
              extendObservable(this, {
                loading: false,
                list: [],
                str: '',
              });
            }
          |]
        expect = [r|
  @observable public list: any = [];
  @observable public loading: any = false;
  @observable public str: any = '';
            loading: any;
            otherField: any;

            constructor() {
              this.reset();
            }

            @action
            protected reset(): void {
              extendObservable(this, {
                loading: false,
                list: [],
                str: '',
              });
            }
          |]
      parse (addClassObsPropertiesParser classContent) "" testStr `shouldParse` expect
