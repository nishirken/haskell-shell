module PropTypes.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Common (Parser)
import PropTypes.Definition (PropType (..), PropTypeDefinition (..))

propTypeParser :: Parser PropType
propTypeParser = undefined

propTypesDefinitionParser :: Parser PropTypeDefinition
propTypesDefinitionParser = undefined
