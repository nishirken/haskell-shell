import Test.Hspec (hspec)

import ConvertImportsSpec (convertImportsSpec)
import CollectPathsSpec (collectPathsSpec)
import InplacePatternsSpec (inplacePatternsSpec)
import ParserSpec (parserSpec)

main :: IO ()
main = hspec $ do
  convertImportsSpec
  collectPathsSpec
  inplacePatternsSpec
  parserSpec
