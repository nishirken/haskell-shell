{-# LANGUAGE OverloadedStrings #-}

module CollectPathsSpec where

import Test.Hspec (describe, it, Spec, shouldBe)
import CollectPaths (makeAbsolute)

collectPathsSpec :: Spec
collectPathsSpec = describe "CollectPathsSpec" $
  it "makeAbsolute" $ do
    let relPaths = ["./src/folder", "./src/anotherFolder"]
    let expectedPaths = ["/Users/someUser/projectPath/src/folder", "/Users/someUser/projectPath/src/anotherFolder"]
    makeAbsolute "/Users/someUser/projectPath" relPaths `shouldBe` expectedPaths
    makeAbsolute "/Users/someUser/projectPath/" relPaths `shouldBe` expectedPaths
