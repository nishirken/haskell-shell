{-# LANGUAGE OverloadedStrings #-}

module RenameFile where

import Turtle
import Prelude hiding (FilePath)
import FileMatchers (isJsxFile)

rename :: FilePath -> IO ()
rename path = do
  isJsx <- isJsxFile path
  mv path $ newPath isJsx
    where
      newPath ::Bool -> FilePath
      newPath isJsx = dropExtension path <.> tsExt isJsx
      tsExt :: Bool -> Text
      tsExt isJsx = if isJsx then "tsx" else "ts"
