module PropTypes.ComponentStatement where

import Data.Text (Text)

data ClassGenerics = ClassGenerics
  { _props :: Text
  , _state :: Maybe Text
  } deriving (Eq, Show)

data ComponentStatement =
  Class { _name :: Text, _generics :: Maybe ClassGenerics }
  | Functional { _name :: Text, _generic :: Maybe Text }
  deriving (Eq, Show)


