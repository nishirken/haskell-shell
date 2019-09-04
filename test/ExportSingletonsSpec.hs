{-# LANGUAGE OverloadedStrings #-}

module ExportSingletonsSpec where

import Test.Hspec (describe, it, Spec)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)
import ExportSingletons (exportSingletonsParser)

exportSingletonsSpec :: Spec
exportSingletonsSpec = describe "ExportSingletons" $ do
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
