{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module EventActor (actorEvent) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import EventMessages
import WebClients

actorEvent :: EventChan -> IO ()
actorEvent chan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                EMWebEvent cs e -> do
                    print $ "send "++ show e ++ " to clients "++ show cs
                    sendToClients cs e
                    loop
                EMStop -> return ()
