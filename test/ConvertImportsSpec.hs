{-# LANGUAGE OverloadedStrings #-}

module ConvertImportsSpec where

import Test.Hspec (describe, context, it, Spec, shouldBe)
import ConvertImports
import Const (projectPath)

convertImportsSpec :: Spec
convertImportsSpec = describe "ConvertImportsSpec" $ do
  it "prepareRelativePaths" $ do
    let
      testData =
        [ "import { classes } from 'typestyle';"
        , "import { outerCardClassName } from '../../CargoOrder/CargoOrder.styles';"
        , "import { CargoStatus } from '../../Cargo/models/CargoStatus';"
        , "import { cargoPageState } from '../CargoPageState';"
        , "interface IAuctionBidWindowProps {"
        , "  className?: string;"
        , "}"
        , ""
        , "const delayAfterNewBid = 1500;"
        ]
    let
      expectData =
        [ PreparedPath "import { outerCardClassName } from '../../CargoOrder/CargoOrder.styles';" "../../"
        , PreparedPath "import { CargoStatus } from '../../Cargo/models/CargoStatus';" "../../"
        , PreparedPath "import { cargoPageState } from '../CargoPageState';" "../"
        ]
    prepareRelativePaths testData `shouldBe` expectData

  it "absolutePrefix" $
    absolutePrefix "/Users/Some/src/folder/inner" "/Users/Some/src/" `shouldBe` "folder/inner"

  it "replacePathLine" $
    shouldBe
      (replacePathLine
        "import { outerCardClassName } from '../../CargoOrder/CargoOrder.styles';"
        "../../" "services/module")
      "import { outerCardClassName } from 'services/module/CargoOrder/CargoOrder.styles';"
