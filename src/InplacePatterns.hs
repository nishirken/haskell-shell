{-# LANGUAGE OverloadedStrings #-}

module InplacePatterns where

import Turtle
import Prelude hiding (FilePath)
import FileMatchers (isJsxFile, isIndexFile)

replace :: FilePath -> IO ()
replace path = do
  isJsx <- isJsxFile path
  inplace (firstRoundPattern isJsx) path
  inplace (secondRoundPattern isJsx) path
    where
      firstRoundPattern isJsx = if isIndexFile path
        then exportDefaultFromPattern
        else if isJsx then jsxFilePatterns else tsPatterns
      secondRoundPattern isJsx = exportDefaultPattern
      tsPatterns = tslintDisabledPattern <|> jsImportPattern
      jsxFilePatterns = tsPatterns <|> genericPattern
      genericPattern = "Component {" *> pure "Component<any, any> {"
      exportDefaultPattern = begins ("export default " *> pure "export ")
      tslintDisabledPattern = "export " *> pure "/* tslint:disable */\nexport "
      jsImportPattern = ends (".js';" *> pure "';")
      exportDefaultFromPattern = begins ("export { default }" *> pure "export *")
