module ProjectActor (actorProject) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (join)
import qualified Data.Map.Strict as M
import System.Exit (die)


import ProjectState
import ProjectMessages
import ViewMessages
import EventMessages
import WebMessages
import LoadMessages
import StoreMessages
import CommonChannels
import WebClients
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
handleRequests chan rp r= case r of
    PRSubscribeToView c vn -> subscribeToView c vn rp chan
    PRUnsubscribeFromView c vn -> unsubscribeFromView c vn rp
    PRLoadMap c mn -> writeTChan (loadChan $ chans rp)  $ LMLoadMap chan c prjName mn
    PRStoreMap c m -> writeTChan (storeChan $ chans rp)  $ StMStoreMap chan c prjName m
    where prjName = projectName $ project rp

handleEvent :: ProjectChan -> RuntimeProject -> ProjectEvent -> IO ()
handleEvent chan rp e = case e of
    PEViewLoaded c v -> viewLoaded rp c v
    PEViewLoadError c vn err -> atomically $ sendError evtChan [c] err
    where evtChan = eventChan $ chans rp

viewLoaded :: RuntimeProject -> WAClient -> View -> IO ()
viewLoaded rp c v = do
     let evtChan  = eventChan $ chans rp
     rv <- atomically $ viewToRuntime (projectName $ project rp) v
     vChan <- viewToChan evtChan rv
     forkIO $ actorView vChan rv evtChan
     atomically $ modifyTVar (viewChanByName rp) (M.insert (viewName v) (Just vChan))
     atomically $ sendSubscriptionToView vChan c

sendSubscriptionToView :: ViewChan -> WAClient -> STM ()
sendSubscriptionToView vchan c = writeTChan vchan (VMSubscribeToView c)

subscribeToView :: WAClient -> ViewName -> RuntimeProject -> ProjectChan -> STM ()
subscribeToView c vn rp chan = do
                vs <- readTVar $ viewChanByName rp
                case M.lookup vn vs of
                    Just maybeVChan -> case maybeVChan of
                                            Just vChan -> sendSubscriptionToView vChan c
                                            Nothing -> writeTChan (loadChan $ chans rp) (LMLoadView chan c (projectName $ project rp) vn)
                    Nothing -> sendStringError (eventChan $ chans rp) [c] ("view " ++ show vn ++ " to subscribe to not found in project " ++ show (projectName $ project rp))

unsubscribeFromView :: WAClient -> ViewName -> RuntimeProject -> STM ()
unsubscribeFromView c vn rp = do
                vs <- readTVar $ viewChanByName rp
                let maybeVChan = M.lookup vn vs
                case join maybeVChan of
                    Just vChan -> writeTChan vChan (VMUnsubscribeFromView c)
                    Nothing -> sendStringError  (eventChan $ chans rp) [c] ("view " ++ show vn ++ " to unsubscribe from not found in project " ++ show (projectName $ project rp))
