{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PropTypesSpec where

import Test.Hspec (describe, it, Spec, context)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import Data.Text (pack)
import Text.RawString.QQ (r)
import PropTypes.Statement (PropType (..), PropTypeStatement (..), StaticPropType (..))
import PropTypes.Parser (propTypeParser, objectOf, propTypeStatementsParser)

propTypesSpec :: Spec
propTypesSpec = describe "PropTypes" $ do
  context "object parser" $ do
    it "one row" $ do
      let
        testParser = pack <$> M.some MC.alphaNumChar
        testStr = "{ field1: 232323, field2: variable }"
        expect = [("field1", "232323"), ("field2", "variable")]
      parse (objectOf $ testParser) "" testStr `shouldParse` expect
    it "several rows" $ do
      let
        testStr = [r|{
          field1: 2323444,
          field2: variable,
        }|]
        expect = [("field1", "2323444"), ("field2", "variable")]
      parse (objectOf $ pack <$> M.some MC.alphaNumChar) "" testStr `shouldParse` expect
  context "prop type parser" $ do
    it "any1" $ parse propTypeParser "" "PropTypes.any" `shouldParse` PropTypeStatement Any False
    it "any2" $ parse propTypeParser "" "any.isRequired" `shouldParse` PropTypeStatement Any True
    it "func" $ parse propTypeParser "" "PropTypes.func" `shouldParse` PropTypeStatement Func False
    it "instanceOf" $
      parse propTypeParser "" "PropTypes.instanceOf(MyComponent)" `shouldParse` PropTypeStatement (InstanceOf "MyComponent") False
    it "oneOf" $
      parse propTypeParser "" "PropTypes.oneOf([  'First', \"Second\"  ])" `shouldParse` PropTypeStatement (OneOf ["First", "Second"]) False
    it "oneOfType" $
      parse propTypeParser "" "PropTypes.oneOfType([PropTypes.number, PropTypes.string])" `shouldParse` PropTypeStatement (OneOfType [PropTypeStatement Number False, PropTypeStatement String False]) False
    it "arrayOf" $
      parse propTypeParser "" "PropTypes.arrayOf(PropTypes.string)" `shouldParse` PropTypeStatement (ArrayOf (PropTypeStatement String False)) False
    it "objectOf" $
      parse propTypeParser "" "PropTypes.objectOf(PropTypes.string)" `shouldParse` PropTypeStatement (ObjectOf (PropTypeStatement String False)) False
    it "shape" $
      parse
        propTypeParser
        ""
        "PropTypes.shape({ field1: PropTypes.string, field2: PropTypes.arrayOf(PropTypes.string) })"
      `shouldParse` PropTypeStatement (Shape [("field1", PropTypeStatement String False), ("field2", PropTypeStatement (ArrayOf (PropTypeStatement String False)) False)]) False
    it "exact" $ do
      let
        testStr = [r|PropTypes.exact({
            field1: PropTypes.any.isRequired,
            field2: PropTypes.arrayOf(PropTypes.string),
            field3: PropTypes.shape({
              field1: PropTypes.number,
              field2: PropTypes.oneOf([ 'First', 'Second' ]),
            })
          })
        |]
        expect = PropTypeStatement (Exact
          [ ("field1", PropTypeStatement Any True)
          , ("field2", PropTypeStatement (ArrayOf (PropTypeStatement String False)) False)
          , ("field3", PropTypeStatement (Shape [("field1", PropTypeStatement Number False), ("field2", PropTypeStatement (OneOf ["First", "Second"]) False)]) False)
          ]) False
      parse propTypeParser "" testStr `shouldParse` expect
  context "statement parser" $ do
    it "single row" $ do
      let
        testStr = "static propTypes = { prop1: PropTypes.any.isRequired, prop2: func, prop3: PropTypes.number };"
        expect =
          [ ("prop1", PropTypeStatement Any True)
          , ("prop2", PropTypeStatement Func False)
          , ("prop3", PropTypeStatement Number False)
          ]
      parse propTypeStatementsParser "" testStr `shouldParse` expect
    it "several rows" $ do
      let
        testStr = [r|static propTypes = {
          prop1: PropTypes.shape({
            field1: PropTypes.number,
            field2: PropTypes.oneOf([ 'First', 'Second' ]),
          }).isRequired,
          prop2: PropTypes.oneOf(['First', 'Second'])
        };|]
        expect =
          [ ("prop1", PropTypeStatement
            (Shape [("field1", PropTypeStatement Number False), ("field2", PropTypeStatement (OneOf ["First", "Second"]) False)]) True)
          , ("prop2", PropTypeStatement (OneOf ["First", "Second"]) False)
          ]
      parse propTypeStatementsParser "" testStr `shouldParse` expect
  context "static prop parser" $ do
    it "with class" $ do
      let
        testStr = [r|static propTypes = {
          // Можно объявить проп на соответствие определённому JS-типу.
          // По умолчанию это не обязательно.
          optionalArray: PropTypes.array,
          optionalBool: PropTypes.bool,
          optionalFunc: PropTypes.func,
          optionalNumber: PropTypes.number,
          optionalObject: PropTypes.object,
          optionalString: PropTypes.string,
          optionalSymbol: PropTypes.symbol,
        
          // Все, что может быть отрендерено:
          // числа, строки, элементы или массивы
          // (или фрагменты) содержащие эти типы
          optionalNode: PropTypes.node,
        
          // React-элемент
          optionalElement: PropTypes.element,
        
          // Тип React-элемент (например, MyComponent).
          optionalElementType: PropTypes.elementType,
          
          // Можно указать, что проп должен быть экземпляром класса
          // Для этого используется оператор `instanceof`.
          optionalMessage: PropTypes.instanceOf(Message),
        
          // Вы можете задать ограничение конкретными значениями
          // при помощи перечисления
          optionalEnum: PropTypes.oneOf(['News', 'Photos']),
        
          // Объект, одного из нескольких типов
          optionalUnion: PropTypes.oneOfType([
            PropTypes.string,
            PropTypes.number,
            PropTypes.instanceOf(Message)
          ]),
        
          // Массив объектов конкретного типа
          optionalArrayOf: PropTypes.arrayOf(PropTypes.number),
        
          // Объект со свойствами конкретного типа
          optionalObjectOf: PropTypes.objectOf(PropTypes.number),
        
          // Объект с определённой структурой
          optionalObjectWithShape: PropTypes.shape({
            color: PropTypes.string,
            fontSize: PropTypes.number
          }),
          
          // Объект со строгой структурой,
          // при наличии необъявленных свойств будут сформированы предупреждения
          optionalObjectWithStrictShape: PropTypes.exact({
            name: PropTypes.string.isRequired,
            quantity: PropTypes.number
          }),   
        
          // Можно добавить`isRequired` к любому из приведённому выше типу,
          // чтобы показывать предупреждение,
          // если проп не передан
          requiredFunc: PropTypes.func.isRequired,
        
          // Значение любого типа
          requiredAny: PropTypes.any.isRequired,
        };|]
        expect =
          [ ("optionalArray", PropTypeStatement Array False)
          , ("optionalBool", PropTypeStatement Bool False)
          , ("optionalFunc", PropTypeStatement Func False)
          , ("optionalNumber", PropTypeStatement Number False)
          , ("optionalObject", PropTypeStatement Object False)
          , ("optionalString", PropTypeStatement String False)
          , ("optionalSymbol", PropTypeStatement Symbol False)
          , ("optionalNode", PropTypeStatement Node False)
          , ("optionalElement", PropTypeStatement Element False)
          , ("optionalElementType", PropTypeStatement ElementType False)
          , ("optionalMessage", PropTypeStatement (InstanceOf "Message") False)
          , ("optionalEnum", PropTypeStatement (OneOf ["News", "Photos"]) False)
          , ("optionalUnion", PropTypeStatement (OneOfType [PropTypeStatement String False, PropTypeStatement Number False, PropTypeStatement (InstanceOf "Message") False]) False)
          , ("optionalArrayOf", PropTypeStatement (ArrayOf (PropTypeStatement Number False)) False)
          , ("optionalObjectOf", PropTypeStatement (ObjectOf (PropTypeStatement Number False)) False)
          , ("optionalObjectWithShape", PropTypeStatement (Shape
            [ ("color", PropTypeStatement String False)
            , ("fontSize", PropTypeStatement Number False)
            ]) False)
          , ("optionalObjectWithStrictShape", PropTypeStatement (Exact
            [ ("name", PropTypeStatement String True)
            , ("quantity", PropTypeStatement Number False)
            ]) False)
          , ("requiredFunc", PropTypeStatement Func True)
          , ("requiredAny", PropTypeStatement Any True)
          ]
      parse propTypeStatementsParser "" testStr `shouldParse` expect
