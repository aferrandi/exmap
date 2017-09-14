module ProjectActor (actorProject) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (join)
import qualified Data.Map.Strict as M
import System.Exit (die)


import TChans
import ProjectState
import ProjectMessages
import ViewMessages
import EventMessages
import WebMessages
import LoadMessages
import StoreMessages
import CommonChannels
import CalculationMessages
import WebClients
import XMapTypes
import View
import Project
import Errors
import Load
import ViewBuild
import ViewActor

actorProject :: ProjectChan -> RuntimeProject -> IO ()
actorProject chan rp = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                PMRequest r -> do
                    atomically $ handleRequests chan rp r
                    loop
                PMEvent e -> do
                    handleEvent chan rp e
                    loop
                PMStop -> return ()
                otherwise -> die $ "Unexpected message " ++ show msg ++ " in project actor"

handleRequests:: ProjectChan -> RuntimeProject -> ProjectRequest -> STM ()
handleRequests chan rp r= do
    pn <- prjName rp
    case r of
        PRSubscribeToProject c -> subscribeToProject c rp
        PRUnsubscribeFromProject c -> unsubscribeFromProject c rp
        PRSubscribeToView c vn -> subscribeToView c vn rp chan
        PRUnsubscribeFromView c vn -> unsubscribeFromView c vn rp
        PRLoadMap c mn -> writeTChan (loadChan $ chans rp)  $ LMLoadMap chan c pn mn
        PRStoreMap c m -> writeTChan (storeChan $ chans rp)  $ StMStoreMap chan c pn  m

handleEvent :: ProjectChan -> RuntimeProject -> ProjectEvent -> IO ()
handleEvent chan rp e = case e of
    PEViewLoaded c v -> viewLoaded rp c v
    PEViewLoadError c vn err -> atomically $ sendError evtChan [c] err
    PEMapLoaded c m -> atomically $ mapLoaded rp c m
    PEMapLoadError c mn err -> atomically $ sendError evtChan [c] err
    PEMapStored c m -> atomically $ mapStored rp c m
    PEMapStoreError c m err -> atomically $ sendError evtChan [c] err
    where evtChan  = eventChan $ chans rp

subscribeToProject :: WAClient -> RuntimeProject -> STM()
subscribeToProject c rp = do
    p <- readTVar $ project rp
    modifyTVar (subscribedClients rp) (\cs -> c : cs)
    writeTChan (eventChan $ chans rp) (EMWebEvent [c] (WEProjectContent p))

unsubscribeFromProject :: WAClient -> RuntimeProject -> STM()
unsubscribeFromProject c rp = do
    modifyTVar (subscribedClients rp) (filter (\ci -> ci /= c))

viewLoaded :: RuntimeProject -> WAClient -> View -> IO ()
viewLoaded rp c v = do
     let evtChan  = eventChan $ chans rp
     pn <- atomically $ prjName rp
     rv <- atomically $ viewToRuntime pn v
     vChan <- viewToChan evtChan rv
     forkIO $ actorView vChan rv evtChan
     atomically $ modifyTVar (viewChanByName rp) (M.insert (viewName v) (Just vChan))
     atomically $ sendSubscriptionToView vChan c

sendSubscriptionToView :: ViewChan -> WAClient -> STM ()
sendSubscriptionToView vchan c = writeTChan vchan (VMSubscribeToView c)

subscribeToView :: WAClient -> ViewName -> RuntimeProject -> ProjectChan -> STM ()
subscribeToView c vn rp chan = do
    vs <- readTVar $ viewChanByName rp
    pn <- prjName rp
    case M.lookup vn vs of
        Just maybeVChan -> case maybeVChan of
            Just vChan -> sendSubscriptionToView vChan c
            Nothing -> writeTChan (loadChan $ chans rp) (LMLoadView chan c pn vn)
        Nothing -> sendStringError (eventChan $ chans rp) [c] ("view " ++ show vn ++ " to subscribe to not found in project " ++ show pn)

unsubscribeFromView :: WAClient -> ViewName -> RuntimeProject -> STM ()
unsubscribeFromView c vn rp = do
    vs <- readTVar $ viewChanByName rp
    pn <- prjName rp
    let maybeVChan = M.lookup vn vs
    case join maybeVChan of
        Just vChan -> writeTChan vChan (VMUnsubscribeFromView c)
        Nothing -> sendStringError  (eventChan $ chans rp) [c] ("view " ++ show vn ++ " to unsubscribe from not found in project " ++ show pn)

mapLoaded :: RuntimeProject -> WAClient -> XNamedMap -> STM ()
mapLoaded rp c m = do
     let evtChan  = eventChan $ chans rp
     pn <- prjName rp
     writeTChan evtChan (EMWebEvent [c] $ WEMapLoaded pn m)

mapStored :: RuntimeProject -> WAClient -> XNamedMap -> STM ()
mapStored rp c m = do
     let mn = xmapName m
     pn <- prjName rp
     cbm <- readTVar $ calculationChanByMap rp
     mapM_ (flip sendToAll (CMMap m) ) (M.lookup mn cbm)
     let evtChan  = eventChan $ chans rp
     writeTChan evtChan (EMWebEvent [c] $ WEMapStored pn mn)


prjName :: RuntimeProject -> STM ProjectName
prjName rp = do
    p <- readTVar (project rp)
    return $ projectName p