{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module EventActor (actorEvent) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Control.Exception as EX

import EventMessages
import LogMessages
import WebClients
import Errors

catchAny :: IO a -> (EX.SomeException -> IO a) -> IO a
catchAny = EX.catch

actorEvent :: EventChan -> LogChan -> IO ()
actorEvent chan lch = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                EMWebEvent cs e -> do
                    logDbg $ "send "++ take 200 (show e) ++ " to clients "++ show cs
                    catchAny (sendToClients cs e)
                        $ \ex -> logErr (mkError $ "Sending to clients got: " ++ show ex)
                    loop
                EMStop -> return ()
          logDbg t = atomically $ logDebug lch t
          logErr e = atomically $ logError lch e
