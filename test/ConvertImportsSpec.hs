{-# LANGUAGE OverloadedStrings #-}

module ConvertImportsSpec where

import Test.Hspec (describe, context, it, Spec, shouldBe)
import ConvertImports

convertImportsSpec :: Spec
convertImportsSpec = describe "ConvertImports" $ do
  it "prepareRelativePaths" $ do
    let
      testData =
        [ "import { Component } from 'react';"
        , "import { oneConst } from '../../components/constants';"
        , "import { status } from '../../models/status';"
        , "import { state } from '../State';"
        , "interface Props {"
        , "  className?: string;"
        , "}"
        , ""
        ]
    let
      expectData =
        [ PreparedPath "import { oneConst } from '../../components/constants';" "../../"
        , PreparedPath "import { status } from '../../models/status';" "../../"
        , PreparedPath "import { state } from '../State';" "../"
        ]
    prepareRelativePaths testData `shouldBe` expectData

  it "absolutePrefix" $
    absolutePrefix "/Users/Some/src/folder/inner" "/Users/Some/src/" `shouldBe` "folder/inner"

  it "replacePathLine" $
    shouldBe
      (replacePathLine
        "import { State } from '../../states/State';"
        "../../" "services/module")
      "import { State } from 'services/module/states/State';"
