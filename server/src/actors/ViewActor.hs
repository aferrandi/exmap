module ViewActor(actorView) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import ViewState
import ViewMessages
import WebClients
import WebMessages
import EventMessages

actorView :: ViewChan -> RuntimeView -> EventChan -> STM ()
actorView chan rv evtChan = loop
    where loop = do
            msg <- readTChan chan
            case msg of
                VMMap m -> do
                    -- the view does not contain the maps, so we just need to send it to all clients
                    cs <- clients
                    writeTChan evtChan (EMWebEvent cs $ WEViewChanged m)
                    loop
                VMError e -> do
                    cs <- clients
                    writeTChan evtChan (EMWebEvent cs $ WEError e)
                VMStop -> return ()
          clients = readTVar (subscribedClients rv)


