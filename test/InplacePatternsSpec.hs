{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module InplacePatternsSpec where

import Test.Hspec (describe, it, Spec, shouldBe)
import InplacePatterns

inplacePatternsSpec :: Spec
inplacePatternsSpec = describe "InplacePatterns" $ do
  it "exportDefault in index files" $ testOnFiles "index.js" "index.expect.js" replaceExportDefaultFrom
  it "js extension in imports" $ testOnFiles "jsImport.js" "jsImport.expect.js" replaceJsExtensionInImports
  it "tslintDisable" $ testOnFiles "tslintDisable.tsx" "tslintDisable.expect.tsx" addTslintDisabled
  it "genericsStub" $ testOnFiles "genericsStub.tsx" "genericsStub.expect.tsx" addComponentGenericsStub
  it "singletonExport" $ testOnFiles "singleton.tsx" "singleton.expect.tsx" replaceExportDefaultSingletons
