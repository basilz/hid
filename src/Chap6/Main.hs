{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Text.IO as TIO
import Options.Applicative as Opt
import TextShow
import System.PosixCompat.Files

import DirTree
import DiskUsage
import AppRTWTST
import AppTypes
import Control.Monad.RWS
import Utils

treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp, n) = fromString indent <> fromString fp
  where
    indent = replicate (2 * n) ' '

