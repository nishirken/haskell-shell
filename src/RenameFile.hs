{-# LANGUAGE OverloadedStrings #-}

module RenameFile where

import Turtle
import Prelude hiding (FilePath)
import ProcessPaths (isJsxFile)
import System.Process (callCommand)

renameToTs :: FilePath -> IO ()
renameToTs path = do
  isJsx <- isJsxFile path
  let gitMvCmd = "git mv " <> encodeString path <> " " <> encodeString (newPath isJsx)
  callCommand gitMvCmd
    where
      newPath ::Bool -> FilePath
      newPath isJsx = dropExtension path <.> tsExt isJsx
      tsExt :: Bool -> Text
      tsExt isJsx = if isJsx then "tsx" else "ts"
