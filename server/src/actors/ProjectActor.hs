module ProjectActor where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import ProjectState
import ProjectMessages
import EventMessages

actorProject :: ProjectChan -> RuntimeProject -> EventChan -> IO ()
actorProject chan project evtChan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                PMStop -> return ()
