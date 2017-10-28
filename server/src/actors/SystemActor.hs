{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module SystemActor (actorSystem) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import SystemState
import SystemMessages
import SystemActorRequests (handleRequest)
import SystemActorEvents (handleEvent)

actorSystem :: SystemChan -> RuntimeSystem -> IO ()
actorSystem chan sys = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                SMRequest r -> do
                    print $ "handling system request " ++ show r
                    atomically $ handleRequest chan sys r
                    loop
                SMEvent e -> do
                    print $ "handling system event " ++ show e
                    handleEvent chan sys e
                    loop
                SMStop -> return ()
