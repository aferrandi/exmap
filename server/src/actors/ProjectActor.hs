module ProjectActor where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import ProjectState
import ActorMessages

actorProject :: ProjectChan -> RuntimeProject -> IO ()
actorProject chan project = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                PMStop -> return ()
