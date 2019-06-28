{-# LANGUAGE OverloadedStrings #-}

module InplacePatterns where

import Turtle
import Prelude hiding (FilePath)
import FileMatchers (isJsxFile, isIndexFile)
import qualified Data.Text as Text
import qualified System.IO.Strict as StrictIO

replace :: FilePath -> IO ()
replace path = do
  isJsx <- isJsxFile path
  inplace (firstRoundPattern isJsx) path
  inplace (secondRoundPattern isJsx) path
    where
      firstRoundPattern isJsx = if isJsx then jsxFilePatterns else tsPatterns
      secondRoundPattern isJsx = exportDefaultPattern
      tsPatterns = tslintDisabledPattern <|> jsImportPattern
      jsxFilePatterns = tsPatterns <|> genericPattern
      genericPattern = "Component {" *> pure "Component<any, any> {"
      exportDefaultPattern = begins ("export default " *> pure "export ")
      tslintDisabledPattern = "export " *> pure "/* tslint:disable */\nexport "
      jsImportPattern = ends (".js';" *> pure "';")

replaceExportDefaultFrom :: FilePath -> IO ()
replaceExportDefaultFrom path = do
  if isIndexFile path then inplace ("export { default }" *> pure "export *") path else pure ()

replaceJsExtensionInImports :: FilePath -> IO ()
replaceJsExtensionInImports = inplace (".js';" *> pure "';")

addTslintDisabled :: FilePath -> IO ()
addTslintDisabled path = do
  content <- StrictIO.readFile $ encodeString path
  writeTextFile path (Text.pack $ "/* tslint:disable */\n" <> content)
