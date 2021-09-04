module AppRTWTST where

import Control.Monad.RWS
import AppTypes

type MyApp logEntry state = RWST AppEnv [logEntry] state IO

runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app config = evalRWST app (initialEnv config)