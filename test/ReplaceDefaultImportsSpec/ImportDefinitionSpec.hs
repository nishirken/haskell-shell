{-# LANGUAGE OverloadedStrings #-}

module ReplaceDefaultImportsSpec.ImportDefinitionSpec where

import Test.Hspec (describe, it, Spec, shouldBe, context)
import ReplaceDefaultImports.ImportDefinition (toJSImport, ImportDefinition )

importDefinitionSpec :: Spec
importDefinitionSpec = describe "ImportDefinition" $ do
  context "toJS" $ do
    it "Named" $
      "" `shouldBe` "2"

