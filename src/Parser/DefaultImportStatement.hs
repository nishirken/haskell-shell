module Parser.DefaultImportStatement where

import Data.Text (Text)

data DefaultImportStatement = DefaultImportStatement
  { _definition :: Text
  , _path :: Text
  } deriving (Eq, Show)
