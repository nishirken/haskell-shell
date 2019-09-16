import Test.Hspec (hspec)

import ConvertImportsSpec (convertImportsSpec)
import ProcessPathsSpec (processPathsSpec)
import InplacePatternsSpec (inplacePatternsSpec)
import ExportSingletonsSpec (exportSingletonsSpec)
import ExtendObservableSpec (extendObservableSpec)
import ReplaceDefaultImportsSpec.StatementSpec (statementSpec)
import ReplaceDefaultImportsSpec.ExportDefinitionSpec (exportDefinitionSpec)
import ReplaceDefaultImportsSpec.ImportDefinitionSpec (importDefinitionSpec)
import ReplaceDefaultImportsSpec.ReplaceSpec (replaceSpec)
import ReplaceDefaultImportsSpec.ResolveImportsSpec (resolveImportsSpec)

main :: IO ()
main = hspec $ do
  convertImportsSpec
  processPathsSpec
  inplacePatternsSpec
  exportSingletonsSpec
  extendObservableSpec
  statementSpec
  exportDefinitionSpec
  importDefinitionSpec
  replaceSpec
  resolveImportsSpec
