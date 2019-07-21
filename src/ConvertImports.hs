{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ConvertImports where

import Turtle
import Prelude hiding (FilePath)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Text as Text
import Const (projectPath)
import Data.Foldable (traverse_)
import InplacePatterns (textLinesFromFile)
import Text.Megaparsec (parse)
import Parser.Statement (Statement (..))
import Data.Either (isRight)

data PreparedPath = PreparedPath
  { originalLine :: Text.Text
  , relativePath :: Text.Text
  } deriving (Eq, Show)

prepareRelativePaths :: [Text.Text] -> [PreparedPath]
prepareRelativePaths = mapMaybe matchLine
    where
      matchLine line = let matched = match importPattern line in
        if (not . null) matched then Just $ PreparedPath line (foldl (<>) "" matched) else Nothing
      importPattern = "import" + "from" + has (selfless "../") + "';\n"

absolutePrefix :: FilePath -> FilePath -> FilePath
absolutePrefix targetPath basePath = fromMaybe "" $ stripPrefix basePath targetPath

replacePathLine :: Text -> Text -> FilePath -> Text
replacePathLine line oldRel newAbs = Text.replace oldRel ((Text.pack . encodeString $ newAbs) <> "/") line

convert :: Turtle.FilePath -> IO ()
convert filePath = do
  content <- readTextFile filePath
  let relParts = prepareRelativePaths $ Text.lines content
  if (not . null) relParts then makeNewContent content relParts >>= writeTextFile filePath else pure ()
    where
      makeNewContent :: Text.Text -> [PreparedPath] -> IO Text
      makeNewContent content [] = pure content
      makeNewContent content [x] = replaceOneImport content x
      makeNewContent content (x:xs) = do
        tempContent <- replaceOneImport content x
        makeNewContent tempContent xs
      replaceOneImport :: Text.Text -> PreparedPath -> IO Text
      replaceOneImport initialContent PreparedPath{..} = do
        cd $ directory filePath
        cd $ fromText relativePath
        importTargetPath <- pwd
        let newAbsolutePrefix = absolutePrefix importTargetPath projectPath
        let newImport = replacePathLine originalLine relativePath newAbsolutePrefix
        pure $ Text.replace originalLine newImport initialContent

replaceDefaultImportStatement :: Turtle.FilePath -> IO ()
replaceDefaultImportStatement filePath = do
  lines <- textLinesFromFile filePath
  pure ()
  
