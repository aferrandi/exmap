module EventActor where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.Text.IO as TIO
import XMapTypes

import EventMessages
import ProjectState
import WebClients

actorEvent :: EventChan -> IO ()
actorEvent chan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                EMWebEvent cs e -> do
                    sendToClients cs e
                    loop
                EMStop -> return ()