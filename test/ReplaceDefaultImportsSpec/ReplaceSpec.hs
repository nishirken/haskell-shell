module ReplaceDefaultImportsSpec.ReplaceSpec where

import Test.Hspec (describe, it, Spec, shouldBe)
import ReplaceDefaultImports.Replace (replaceDefaultImports)
import TestUtils

replaceSpec :: Spec
replaceSpec = describe "Replace" $ do
  it "replaceDefaultImports" $ do
    it "singletonExport" $ testOnFiles "singleton.tsx" "singleton.expect.tsx" replaceExportDefaultSingletons
