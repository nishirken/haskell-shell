module PropTypes.Definition where

import Data.Text (Text)

data PropType
  = Any
  | Func
  | Array
  | Bool
  | Number
  | Object
  | String
  | Symbol
  | Node
  | Element
  | ElementType
  | InstanceOf Text
  | OneOf [Text]
  | OneOfType [PropType]
  | ArrayOf PropType
  | ObjectOf PropType
  | Shape [(Text, PropType)]
  | Exact [(Text, PropType)]
  | NotSupported
  deriving (Eq, Show)

data PropTypeDefinition = PropTypeDefinition
  { _name :: Text
  , _type :: PropType
  , _isRequired :: Bool
  } deriving (Eq, Show)
