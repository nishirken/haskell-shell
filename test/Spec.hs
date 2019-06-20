import Test.Hspec (hspec)

import ConvertImportsSpec (convertImportsSpec)
import CollectPathsSpec (collectPathsSpec)

main :: IO ()
main = hspec $ do
  convertImportsSpec
  collectPathsSpec
