{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PropTypes.ToTs (toTsInterface) where

import PropTypes.Statement (PropType (..), PropTypeStatement (..))
import qualified Data.Text as T

toTsInterface :: [(T.Text, PropTypeStatement)] -> T.Text
toTsInterface xs =
  "export interace " <> objectTransform xs

objectTransform :: [(T.Text, PropTypeStatement)] -> T.Text
objectTransform xs = "{\n" <> T.unlines fields <> "\n}"
  where
    fields :: [T.Text]
    fields = map fieldTransform xs
    fieldTransform :: (T.Text, PropTypeStatement) -> T.Text
    fieldTransform (fieldName, PropTypeStatement{..}) =
      "  "
      <> fieldName
      <> if _isRequired then ":" else "?;"
      <> typeTransform _type

typeTransform :: PropType -> T.Text
typeTransform Any = "any"
typeTransform Func = "(...any[]) => any"
typeTransform Array = "any[]"
typeTransform Bool = "boolean"
typeTransform Number = "number"
typeTransform Object = "object"
typeTransform String = "string"
typeTransform Symbol = "Symbol"
typeTransform Node = "React.ReactNode"
typeTransform Element = "React.ReactElement<any>"
typeTransform ElementType = "React.ComponentType<any>"
typeTransform (InstanceOf className) = className
typeTransform (OneOf xs) = foldr (\acc x -> acc <> "|" <> x) "" xs
typeTransform (OneOfType xs) = foldr (\PropTypeStatement {..} acc -> acc <> "|" <> typeTransform _type) "" xs
typeTransform (ArrayOf PropTypeStatement{..}) = typeTransform _type <> "[]"
typeTransform (ObjectOf PropTypeStatement{..}) = "Record<string | number," <> typeTransform _type <> ">"
typeTransform (Shape statements) = shapeTransform statements
typeTransform (Exact statements) = shapeTransform statements
typeTransform NotSupported = "any"

shapeTransform :: [(T.Text, PropTypeStatement)] -> T.Text
shapeTransform xs = objectTransform xs <> ";"
