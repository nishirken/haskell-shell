{-# LANGUAGE OverloadedStrings #-}

module ReplaceDefaultImportsSpec.ImportDefinitionSpec where

import Test.Hspec (describe, it, Spec, shouldBe, context)

importDefinitionSpec :: Spec
importDefinitionSpec = describe "ImportDefinition" $ do
  context "toJS" $ do
    it "fst" $ do
      "" `shouldBe` "2"

