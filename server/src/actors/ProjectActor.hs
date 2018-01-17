{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProjectActor (actorProject) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import ProjectState
import ProjectMessages
import ProjectActorRequests
import ProjectActorEvents
import LogMessages
import CommonChannels

actorProject :: ProjectChan -> RuntimeProject -> IO ()
actorProject chan rp = loop
    where loop = do
            msg <- atomically $ readTChan chan
            pn <- atomically $ prjName rp
            case msg of
                PMRequest r -> do
                    logDbg $ "handling project request " ++ show r ++ " for project " ++ show pn
                    atomically $ handleRequests chan rp r
                    loop
                PMEvent e -> do
                    logDbg $ "handling project event " ++ show e ++ " for project " ++ show pn
                    handleEvent chan rp e
                    loop
                PMStop -> return ()
          logDbg t = atomically $ logDebug (logChan $ chans rp) t


