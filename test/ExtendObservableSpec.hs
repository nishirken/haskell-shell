{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ExtendObservableSpec where

import Test.Hspec (describe, it, Spec)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)
import qualified Data.Map as M
import Text.RawString.QQ (r)
import ExtendObservable

extendObservableSpec :: Spec
extendObservableSpec = describe "ExtendObservable" $ do
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
