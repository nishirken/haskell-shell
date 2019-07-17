module Parser.NormalImportStatement where

import Data.Text (Text)

data NormalImportStatement = NormalImportStatement
  { _definitions :: [Text]
  , _path :: Text
  } deriving (Eq, Show)
