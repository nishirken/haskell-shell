{-# LANGUAGE OverloadedStrings #-}

module ReplaceDefaultImportsSpec.ExportDefinitionSpec where

import Test.Hspec (describe, it, Spec, shouldBe)

exportDefinitionSpec :: Spec
exportDefinitionSpec = describe "ExportDefinition" $ do
  it "toJS" $ do
    "" `shouldBe` "2"


