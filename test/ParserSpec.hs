{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Test.Hspec (describe, context, it, Spec, shouldBe)
import Text.Megaparsec (parse)
import Parser.ExportSingletons

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

