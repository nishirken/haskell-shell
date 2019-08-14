{-# LANGUAGE OverloadedStrings #-}

module RenameFile where

import Turtle
import Prelude hiding (FilePath)
import FileMatchers (isJsxFile)
import System.Process (callCommand)

rename :: FilePath -> IO ()
rename path = do
  isJsx <- isJsxFile path
  let gitMvCmd = "git mv " <> encodeString path <> " " <> encodeString (newPath isJsx)
  print $ "oldPath: " <> path
  print $ "newPath: " <> newPath isJsx
  callCommand gitMvCmd
    where
      newPath ::Bool -> FilePath
      newPath isJsx = dropExtension path <.> tsExt isJsx
      tsExt :: Bool -> Text
      tsExt isJsx = if isJsx then "tsx" else "ts"
