{-# LANGUAGE NamedFieldPuns #-}

module Utils where

import AppRTWTST (MyApp)
import AppTypes
import Control.Monad.RWS (MonadReader (ask), asks, local)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Foldable
import System.Directory
import System.FilePath
import System.PosixCompat.Files (FileStatus)

currentPathStatus :: MyApp l s FileStatus
currentPathStatus = do
    AppEnv{fileStatus, path} <- ask
    liftIO $ fileStatus path

checkExtension :: AppConfig -> FilePath -> Bool
checkExtension cfg fp =
    maybe True (`isExtensionOf` fp) (extension cfg)

traverseDirectoryWith :: MyApp logEntry s () -> MyApp logEntry s ()
traverseDirectoryWith app =
    asks path >>= liftIO . listDirectory >>= traverse_ go
  where
    go name =
        local
            ( \e ->
                e
                    { path = path e </> name
                    , depth = depth e + 1
                    }
            )
            app