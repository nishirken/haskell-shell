{-# LANGUAGE OverloadedStrings #-}

module ProcessPathsSpec where

import Test.Hspec (describe, it, Spec, shouldBe)
import ProcessPaths (makeAbsolute)

processPathsSpec :: Spec
processPathsSpec = describe "ProcessPathsSpec" $
  it "makeAbsolute" $ do
    let relPaths = ["./src/folder", "./src/anotherFolder"]
    let expectedPaths = ["/Users/someUser/projectPath/src/folder", "/Users/someUser/projectPath/src/anotherFolder"]
    makeAbsolute "/Users/someUser/projectPath" relPaths `shouldBe` expectedPaths
    makeAbsolute "/Users/someUser/projectPath/" relPaths `shouldBe` expectedPaths
