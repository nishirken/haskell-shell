import Test.Hspec (hspec)

import ConvertImportsSpec (convertImportsSpec)
import ProcessPathsSpec (processPathsSpec)
import InplacePatternsSpec (inplacePatternsSpec)
import ExportSingletonsSpec (exportSingletonsSpec)
import ExtendObservableSpec (extendObservableSpec)
import ReplaceDefaultImportsSpec.StatementSpec (statementSpec)

main :: IO ()
main = hspec $ do
  convertImportsSpec
  processPathsSpec
  inplacePatternsSpec
  exportSingletonsSpec
  extendObservableSpec
  statementSpec
