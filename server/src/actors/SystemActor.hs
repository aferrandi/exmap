{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module SystemActor (actorSystem) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import SystemState
import SystemMessages
import SystemActorRequests (handleRequest)
import SystemActorEvents (handleEvent)
import LogMessages
import CommonChannels

actorSystem :: SystemChan -> RuntimeSystem -> IO ()
actorSystem chan sys = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                SMRequest r -> do
                    logDbg $ "handling system request " ++ show r
                    atomically $ handleRequest chan sys r
                    loop
                SMEvent e -> do
                    logDbg $ "handling system event " ++ show e
                    handleEvent chan sys e
                    loop
                SMStop -> return ()
          logDbg t = atomically $ logDebug (logChan $ chans sys) t