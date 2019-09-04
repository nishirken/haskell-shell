module Common where

import Data.Text (Text)
import Text.Megaparsec (Parsec)
import Data.Void (Void)

type Parser = Parsec Void Text
