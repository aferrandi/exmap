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
import LogMessages
import XMapTypes

actorView :: ViewChan -> RuntimeView -> EventChan -> IO ()
actorView chan rv evtChan = atomically loop
    where loop = do
            msg <- readTChan chan
            let vn = runtimeViewName rv
            case msg of
                VMMaps ms -> do
                    logDbg $ "View " ++ show vn ++ " handling VMMaps " ++ show (map xmapName ms)
                    handleMaps rv evtChan ms
                    loop
                VMSubscribeToView c -> do
                    logDbg $ "View " ++ show vn ++ " handling VMSubscribeToView "
                    subscribeToView rv evtChan c
                    loop
                VMUnsubscribeFromView c -> do
                    logDbg $ "View " ++ show vn ++ " handling VMUnsubscribeFromView"
                    unsuscribeFromView rv evtChan c
                    loop
                VMUpdate v -> do
                    logDbg $ "View " ++ show vn ++ " handling VMUpdate " ++ show v
                    handleView rv evtChan v
                    loop
                VMError e -> do
                    logDbg $ "View " ++ show vn ++ " handling VMError " ++ show e
                    sendErrorToClients rv evtChan e
                    loop
                VMStop -> return ()
          logDbg = logDebug (logChan rv)

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
    modifyTVar (mapsInView rv) $ updateMapsInView ms
    cs <- readTVar $ subscribedClients rv
    writeTChan evtChan (EMWebEvent cs $ WEViewChanged (ownerProjectName rv) (runtimeViewName rv) ms)

updateMapsInView :: [XNamedMap] -> XMapByName -> XMapByName
updateMapsInView ms msbn = foldr  (\m msbni -> M.insert (xmapName m) (xmap m) msbni) msbn ms

handleView :: RuntimeView -> EventChan -> View -> STM ()
handleView rv evtChan v = do
    writeTVar (view rv) v
    cs <- readTVar $ subscribedClients rv
    sendStatus rv evtChan cs

sendStatus :: RuntimeView -> EventChan -> [WAClient] -> STM ()
sendStatus rv evtChan cs = do
    v <- readTVar $ view rv
    ms <- readTVar $ mapsInView rv
    let nms = map (\(k,a) -> XNamedMap { xmapName = k ,xmap = a }) $ M.toList ms
    writeTChan evtChan (EMWebEvent cs $ WEViewStatus (ownerProjectName rv)  v nms)


sendErrorToClients :: RuntimeView -> EventChan -> Error -> STM ()
sendErrorToClients  rv evtChan e = do
    cs <- readTVar $ subscribedClients rv
    writeTChan evtChan (EMWebEvent cs $ WEError e)
