module ProjectActorEventsForView where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as B
import Debug.Trace (trace)

import ProjectState
import ProjectMessages
import ViewMessages
import EventMessages
import WebMessages
import LoadMessages
import StoreMessages
import CommonChannels
import CalculationMessages
import Dependencies
import WebClients
import XMapTypes
import View
import Project
import ViewBuild

viewForClientLoaded :: WAClient -> RuntimeProject -> View -> STM ()
viewForClientLoaded c rp v = do
    pn <- prjName rp
    writeTChan (evtChan rp) (EMWebEvent [c] $ WEViewLoaded pn v)

mapsForViewLoaded :: RuntimeProject -> WAClient -> ViewName -> [XNamedMap] -> STM ()
mapsForViewLoaded rp c vn ms = do
    vs <- readTVar $ viewChanByName rp
    pn <- prjName rp
    case M.lookup vn vs of
        Just vchan -> writeTChan (vcChannel vchan) (VMMaps ms)
        Nothing -> sendStringError (evtChan rp) [c] ("view " ++ show vn ++ " to add the maps to not found in project " ++ show pn)

viewAdded :: ProjectChan -> RuntimeProject -> WAClient -> View -> IO ()
viewAdded chan rp c v = do
    p <- readTVarIO $ project rp
    addView chan rp c v
    atomically $ do
        storeProject chan rp c
        writeTChan (evtChan rp) (EMWebEvent [c] $ WEViewAdded (projectName p) (viewName v))

viewUpdated :: ProjectChan -> RuntimeProject -> WAClient -> View -> IO ()
viewUpdated chan rp c v = do
    p <- readTVarIO $ project rp
    atomically $ do
        updateView chan rp c p v
        storeProject chan rp c
        writeTChan (evtChan rp) (EMWebEvent [c] $ WEViewUpdated (projectName p) (viewName v))


sendSubscriptionToView :: ViewChan -> WAClient -> STM ()
sendSubscriptionToView vchan c = writeTChan (vcChannel vchan) (VMSubscribeToView c)

viewForProjectLoaded :: ProjectChan -> RuntimeProject -> WAClient -> View -> IO ()
viewForProjectLoaded chan rp c v = do
     p <- readTVarIO $ project rp
     vch <- viewToChan (evtChan rp) (logChan $ chans rp) (projectName p) v
     atomically $ do addViewToProject rp vch v
                     sendSubscriptionToView vch c
                     sendDependedMapsToView chan rp c v
                     listenToDependentCalculations rp vch v

addViewToProject ::  RuntimeProject -> ViewChan -> View -> STM ()
addViewToProject rp vch v = do
    modifyTVar (viewChanByName rp) $ M.insert (viewName v) vch
    modifyTVar (viewChanByMap rp) $ updateViewChanByMap vch (viewDependenciesMaps v)

addView :: ProjectChan -> RuntimeProject -> WAClient -> View -> IO()
addView chan rp c v = do
    let vn = viewName v
    atomically $ modifyTVar (project rp) (\p -> p { views = vn : views p })
    pn <- atomically $ prjName rp
    vch <- viewToChan (evtChan rp) (logChan $ chans rp) pn v
    atomically $ do
        addViewToProject rp vch v
        writeTChan (vcChannel vch) $ VMUpdate v
        sendDependedMapsToView chan rp c v
        listenToDependentCalculations rp vch v

updateView :: ProjectChan -> RuntimeProject -> WAClient -> Project -> View -> STM()
updateView chan rp c p v = do
    let vn = viewName v
    vbn <- readTVar $ viewChanByName rp
    case M.lookup vn vbn of
        Just vch -> do
            modifyTVar (viewChanByMap rp) $ updateViewChanByMap vch (viewDependenciesMaps v)
            writeTChan (vcChannel vch) (VMUpdate v)
            sendDependedMapsToView chan rp c v
            listenToDependentCalculations rp vch v
        Nothing -> sendStringError (evtChan rp) [c] ("stored view " ++ show vn ++ " not found in project " ++ show (projectName p))

listenToDependentCalculations :: RuntimeProject -> ViewChan -> View -> STM ()
listenToDependentCalculations rp vch v = do
    let deps = viewDependenciesMaps v
    cbr <- readTVar $ calculationByResult rp
    let cs = B.mapMaybe (\d -> M.lookup d cbr) deps
    cbn <- readTVar $ calculationChanByName rp
    let chs = B.mapMaybe (\c -> M.lookup c cbn) cs
    mapM_ (\ch -> writeTChan (ccChannel ch) (CMViewStarted vch)) chs

sendDependedMapsToView :: ProjectChan -> RuntimeProject -> WAClient -> View -> STM ()
sendDependedMapsToView chan rp c v = do
     p <- readTVar $ project rp
     case sourcesOfTypeInProject FileSource p of
        Just fileSources -> do
             let toLoad = L.intersect (viewDependenciesMaps v) fileSources
             writeTChan (loadChan $ chans rp) $ LMLoadMapsForView chan c (projectName p) (viewName v) toLoad
        Nothing -> return ()

updateViewChanByMap :: ViewChan -> [XMapName] -> ViewChanByMap -> ViewChanByMap
updateViewChanByMap vch mns = trace ("updateView:" ++ show mns) $ updated . cleaned
    where cleaned :: ViewChanByMap -> ViewChanByMap
          cleaned = M.map (filter (/= vch))
          updated :: ViewChanByMap -> ViewChanByMap
          updated vbm = foldr addToMultimap vbm mns
          addToMultimap :: XMapName -> ViewChanByMap -> ViewChanByMap
          addToMultimap mn = M.insertWith (++) mn [vch]

storeProject :: ProjectChan -> RuntimeProject -> WAClient -> STM ()
storeProject chan rp c = do
    p <- readTVar $ project rp
    writeTChan (storeChan $ chans rp) (StMStoreExistingProject chan c p)
