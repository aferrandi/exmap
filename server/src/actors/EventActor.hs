{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module EventActor (actorEvent) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Control.Exception as EX

import EventMessages
import WebClients

catchAny :: IO a -> (EX.SomeException -> IO a) -> IO a
catchAny = EX.catch

actorEvent :: EventChan -> IO ()
actorEvent chan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                EMWebEvent cs e -> do
                    print $ "send "++ take 200 (show e) ++ " to clients "++ show cs
                    catchAny (sendToClients cs e)
                        $ \ex -> print $ "Sending to clients got: " ++ show ex
                    loop
                EMStop -> return ()
