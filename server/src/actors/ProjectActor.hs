module ProjectActor where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (join)
import qualified Data.Map.Strict as M


import ProjectState
import ProjectMessages
import ViewMessages
import EventMessages
import WebMessages
import LoadMessages
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
                        handleRequests chan rp r
                        loop
                PMEvent e -> do
                        handleEvent chan rp e
                        loop
                PMStop -> return ()

handleRequests:: ProjectChan -> RuntimeProject -> ProjectRequest -> IO ()
handleRequests chan rp r= case r of
    PRSubscribeToView c vn -> atomically $ subscribeToView c vn rp chan
    PRUnsubscribeFromView c vn -> atomically $ unsubscribeFromView c vn rp

handleEvent :: ProjectChan -> RuntimeProject -> ProjectEvent -> IO ()
handleEvent chan rp e = case e of
    PEViewLoaded c v -> do
        rv <- atomically $ viewToRuntime (projectName $ project rp) v
        vChan <- viewToChan evtChan rv
        forkIO $ atomically (actorView vChan rv evtChan)
        atomically $ modifyTVar (viewChanByName rp) (M.insert (viewName v) (Just vChan))
        atomically $ sendSubscriptionToView vChan c
    PEViewLoadError c vn err -> atomically $ sendError evtChan [c] err
    where evtChan = eventChan $ chans rp

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
