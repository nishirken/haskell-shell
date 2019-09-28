module PropTypes.Statement where

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
  | OneOfType [PropTypeStatement]
  | ArrayOf PropTypeStatement
  | ObjectOf PropTypeStatement
  | Shape [(Text, PropTypeStatement)]
  | Exact [(Text, PropTypeStatement)]
  | NotSupported
  deriving (Eq, Show)

data PropTypeStatement = PropTypeStatement
  { _type :: PropType
  , _isRequired :: Bool
  } deriving (Eq, Show)
