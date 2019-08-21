{-# LANGUAGE OverloadedStrings #-}

module Const where

import Turtle
import Prelude hiding (FilePath)
import System.Directory (getHomeDirectory)

getProjectPath :: IO FilePath
getProjectPath = do
  homeDir <- getHomeDirectory
  pure $ decodeString homeDir </> "Projects/MonopolyOnline.Frontend/frontend/src/"
