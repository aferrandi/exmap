{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProjectActor (actorProject) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import ProjectState
import ProjectMessages
import ProjectActorRequests
import ProjectActorEvents

actorProject :: ProjectChan -> RuntimeProject -> IO ()
actorProject chan rp = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                PMRequest r -> do
                    print $ "handling project request " ++ show r
                    atomically $ handleRequests chan rp r
                    loop
                PMEvent e -> do
                    print $ "handling project event " ++ show e
                    handleEvent chan rp e
                    loop
                PMStop -> return ()



