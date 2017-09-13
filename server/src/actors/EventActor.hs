module EventActor (actorEvent) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.Text.IO as TIO
import Data.Aeson
import System.Exit (die)

import XMapTypes
import EventMessages
import ProjectState
import WebClients
import WebMessagesJson

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
                otherwise -> die $ "Unexpected message " ++ show msg ++ " in event actor"