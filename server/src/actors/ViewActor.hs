{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ViewActor(actorView) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM

import View
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
                VMMaps ms -> do
                    atomically $ handleMaps rv evtChan ms
                    loop
                VMSubscribeToView c -> do
                    atomically $ subscribeToView rv evtChan c
                    loop
                VMUnsubscribeFromView c -> do
                    atomically $ unsuscribeFromView rv evtChan c
                    loop
                VMUpdate v -> do
                    atomically $ handleView rv evtChan v
                    loop
                VMError e -> do
                    atomically $ sendErrorToClients rv evtChan e
                    loop
                VMStop -> return ()


subscribeToView :: RuntimeView -> EventChan -> WAClient -> STM ()
subscribeToView rv evtChan c = do
    modifyTVar (subscribedClients rv) (\cs -> c : cs)
    sendStatus rv evtChan [c]

unsuscribeFromView :: RuntimeView -> EventChan -> WAClient -> STM ()
unsuscribeFromView rv evtChan c = do
    let projectName = ownerProjectName rv
    modifyTVar (subscribedClients rv) (filter (not . sameClientId c))
    writeTChan evtChan (EMWebEvent [c] $ WEUnsubscribedFromView projectName (runtimeViewName rv))

handleMaps :: RuntimeView -> EventChan -> [XNamedMap] -> STM ()
handleMaps rv evtChan ms = do
    -- the view does not contain the maps, so we just need to send it to all clients
    cs <- readTVar $ subscribedClients rv
    writeTChan evtChan (EMWebEvent cs $ WEViewChanged (ownerProjectName rv) (runtimeViewName rv) ms)

handleView :: RuntimeView -> EventChan -> View -> STM ()
handleView rv evtChan v = do
    writeTVar (view rv) v
    cs <- readTVar $ subscribedClients rv
    sendStatus rv evtChan cs

sendStatus :: RuntimeView -> EventChan -> [WAClient] -> STM ()
sendStatus rv evtChan cs = do
    v <- readTVar $ view rv
    ms <- readTVar $ status rv
    let nms = map (\(k,a) -> XNamedMap { xmapName = k ,xmap = a }) $ M.toList ms
    writeTChan evtChan (EMWebEvent cs $ WEViewStatus (ownerProjectName rv)  v nms)


sendErrorToClients :: RuntimeView -> EventChan -> Error -> STM ()
sendErrorToClients  rv evtChan e = do
    cs <- readTVar $ subscribedClients rv
    writeTChan evtChan (EMWebEvent cs $ WEError e)
