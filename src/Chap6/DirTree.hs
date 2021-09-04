{-# LANGUAGE RecordWildCards #-}

module DirTree where

import AppRTWTST
import AppTypes
import Control.Monad.RWS
import System.FilePath
import System.PosixCompat.Files
import Utils

dirTree :: MyApp (FilePath, Int) s ()
dirTree = do
    AppEnv{..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
        tell [(takeBaseName path, depth)]
        traverseDirectoryWith dirTree

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
    AppEnv {..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
        traverseDirectoryWith fileCount
        files <- liftIO $ listFiles path
        tell [(path, length $ filter (checkExtension cfg) files)]