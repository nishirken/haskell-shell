{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PropTypesSpec where

import Test.Hspec (describe, it, Spec, context)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)
import PropTypes.Statement (PropType (..))
import PropTypes.Parser (propTypeParser)

propTypesSpec :: Spec
propTypesSpec = describe "PropTypes" $ do
  context "prop type parser" $ do
    it "any1" $ parse propTypeParser "" "PropTypes.any" `shouldParse` Any
    it "any2" $ parse propTypeParser "" "any.isRequired" `shouldParse` Any
    it "func" $ parse propTypeParser "" "PropTypes.func" `shouldParse` Func
    it "instanceOf" $
      parse propTypeParser "" "PropTypes.instanceOf(MyComponent)" `shouldParse` InstanceOf "MyComponent"
    it "oneOf" $
      parse propTypeParser "" "PropTypes.oneOf(['First', \"Second\"])" `shouldParse` OneOf ["First", "Second"]
    it "oneOfType" $
      parse propTypeParser "" "PropTypes.oneOfType([PropTypes.number, PropTypes.string])" `shouldParse` OneOfType [Number, String]
    it "arrayOf" $
      parse propTypeParser "" "PropTypes.arrayOf(PropTypes.string)" `shouldParse` ArrayOf String
    it "objectOf" $
      parse propTypeParser "" "PropTypes.objectOf(PropTypes.string)" `shouldParse` ObjectOf String
    it "shape" $
      parse
        propTypeParser
        ""
        "PropTypes.shape({ field1: PropTypes.string, field2: PropTypes.arrayOf(PropTypes.String) })"
      `shouldParse` Shape [("field1", String), ("field2", ArrayOf String)]
    it "exact" $ do
      let
        testStr = [r|
          PropTypes.exact({
            field1: PropTypes.any.isRequired,
            field2: PropTypes.arrayOf(PropTypes.String).isRequired,
            field3: PropTypes.shape({
              field1: PropTypes.number,
              field2: PropTypes.oneOf([ 'First', 'Second' ]),
            })
          })
        |]
        expect = Exact
          [ ("field1", Any)
          , ("field2", ArrayOf String)
          , ("field3", Shape [("field1", Number), ("field2", OneOf ["First", "Second"])])
          ]
      parse propTypeParser "" testStr `shouldParse` expect
