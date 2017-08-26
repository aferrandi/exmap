module ViewActor(actorView) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import System.Exit (die)

import ViewState
import ViewMessages
import WebClients
import WebMessages
import EventMessages
import XMapTypes

actorView :: ViewChan -> RuntimeView -> EventChan -> IO ()
actorView chan rv evtChan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                VMMap m -> do
                    atomically $ handleMap m rv evtChan
                    loop
                VMSubscribeToView c -> do
                    atomically $ subscribeToView c rv evtChan
                    loop
                VMUnsubscribeFromView c -> do
                    atomically $ unsuscribeFromView c rv evtChan
                    loop
                VMError e -> do
                    atomically $ sendErrorToClients e rv evtChan
                    loop
                VMStop -> return ()
                otherwise -> die $ "Unexpected message " ++ show msg ++ " in view actor"


subscribeToView :: WAClient -> RuntimeView -> EventChan -> STM ()
subscribeToView c rv evtChan = do
    let projectName = ownerProjectName rv
    modifyTVar (subscribedClients rv) (\cs -> c : cs)
    v <- readTVar $ view rv
    ms <- readTVar $ status rv
    writeTChan evtChan (EMWebEvent [c] $ WEViewStatus projectName v (map (\(k,a) -> XNamedMap { xmapName = k ,xmap = a }) $ M.toList ms))


unsuscribeFromView :: WAClient -> RuntimeView -> EventChan -> STM ()
unsuscribeFromView c rv evtChan = do
    let projectName = ownerProjectName rv
    modifyTVar (subscribedClients rv) (filter (not . sameClientId c))
    writeTChan evtChan (EMWebEvent [c] $ WEUnsubscribedFromView projectName (runtimeViewName rv))

handleMap :: XNamedMap -> RuntimeView -> EventChan -> STM ()
handleMap m rv evtChan = do
        -- the view does not contain the maps, so we just need to send it to all clients
        let projectName = ownerProjectName rv
        cs <- readTVar $ subscribedClients rv
        writeTChan evtChan (EMWebEvent cs $ WEViewChanged projectName (runtimeViewName rv) m)

sendErrorToClients :: Error -> RuntimeView -> EventChan -> STM ()
sendErrorToClients  e rv evtChan = do
    cs <- readTVar $ subscribedClients rv
    writeTChan evtChan (EMWebEvent cs $ WEError e)
