module ViewActor(actorView) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import ViewState
import ViewMessages
import WebClients
import WebMessages
import EventMessages
import XMapTypes

actorView :: ViewChan -> RuntimeView -> EventChan -> STM ()
actorView chan rv evtChan = loop
    where loop = do
            msg <- readTChan chan
            case msg of
                VMMap m -> do
                    -- the view does not contain the maps, so we just need to send it to all clients
                    cs <- readTVar clients
                    writeTChan evtChan (EMWebEvent cs $ WEViewChanged projectName thisViewName m)
                    loop
                VMSubscribeToView c -> do
                    modifyTVar clients (\cs -> c : cs)
                    v <- readTVar $ view rv
                    ms <- readTVar $ status rv
                    writeTChan evtChan (EMWebEvent [c] $ WEViewStatus projectName v (map (\(k,a) -> XNamedMap { xmapName = k ,xmap = a }) $ M.toList ms))
                    loop
                VMUnsubscribeFromView c -> do
                    modifyTVar clients (filter (not . sameClientId c))
                    writeTChan evtChan (EMWebEvent [c] $ WEUnsubscribedFromView projectName thisViewName)
                    loop
                VMError e -> do
                    cs <- readTVar clients
                    writeTChan evtChan (EMWebEvent cs $ WEError e)
                VMStop -> return ()
          clients =  subscribedClients rv
          projectName = ownerProjectName rv
          thisViewName = runtimeViewName rv


