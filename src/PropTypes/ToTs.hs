{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PropTypes.ToTs (toTsInterface) where

import PropTypes.Statement (PropType (..), PropTypeStatement (..))
import qualified Data.Text as T
import Data.Foldable (fold)

toTsInterface :: T.Text -> [(T.Text, PropTypeStatement)] -> T.Text
toTsInterface propsName xs =
  "export interface " <> propsName <> " " <> objectTransform 2 xs

objectTransform :: Int -> [(T.Text, PropTypeStatement)] -> T.Text
objectTransform indent xs = "{\n" <> T.unlines fields <> "}"
  where
    fields :: [T.Text]
    fields = map fieldTransform xs
    fieldTransform :: (T.Text, PropTypeStatement) -> T.Text
    fieldTransform (fieldName, PropTypeStatement{..}) =
      fold (replicate indent " ")
      <> fieldName
      <> if _isRequired then ": " else "?: "
      <> typeTransform indent _type
      <> ";"

typeTransform :: Int -> PropType -> T.Text
typeTransform _ Any = "any"
typeTransform _ Func = "(...any[]) => any"
typeTransform _ Array = "any[]"
typeTransform _ Bool = "boolean"
typeTransform _ Number = "number"
typeTransform _ Object = "object"
typeTransform _ String = "string"
typeTransform _ Symbol = "Symbol"
typeTransform _ Node = "React.ReactNode"
typeTransform _ Element = "React.ReactElement<any>"
typeTransform _ ElementType = "React.ComponentType<any>"
typeTransform _ (InstanceOf className) = className
typeTransform _ (OneOf xs) = foldr (\acc x -> acc <> " | " <> ("'" <> x <> "'")) "" xs
typeTransform indent (OneOfType xs) =
  foldr (\PropTypeStatement {..} acc -> acc <> " | " <> typeTransform indent _type) "" xs
typeTransform indent (ArrayOf PropTypeStatement{..}) = typeTransform indent _type <> "[]"
typeTransform indent (ObjectOf PropTypeStatement{..}) = "Record<string | number," <> " " <> typeTransform indent _type <> ">"
typeTransform indent (Shape statements) = objectTransform (indent + indent) statements
typeTransform indent (Exact statements) = objectTransform (indent + indent) statements
typeTransform _ NotSupported = "any"
