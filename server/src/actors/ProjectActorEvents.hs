module ProjectActorEvents where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.List as L
import qualified Data.Map.Strict as M

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
import Dependencies
import WebClients
import XMapTypes
import View
import Project
import Calculation
import CalculationBuild
import ViewBuild
import FormulaText

handleEvent :: ProjectChan -> RuntimeProject -> ProjectEvent -> IO ()
handleEvent chan rp e = case e of
    PEViewLoaded c v -> atomically $ viewLoaded c rp v
    PEViewLoadError c _ err -> atomically $ sendError ec [c] err
    PEMapsLoaded c ms -> atomically $ mapsLoaded rp c ms
    PEMapsLoadError c _ err -> atomically $ sendError ec [c] err
    PEMapsForViewLoaded c vn ms -> atomically $ mapsForViewLoaded rp c vn ms
    PEMapsForViewLoadError c _ _ err -> atomically $ sendError ec [c] err
    PEMapStored c m -> atomically $ mapStored chan rp c m
    PEMapStoreError c _ err -> atomically $ sendError ec [c] err
    PECalculationStored c cc -> calculationStored chan rp c cc
    PECalculationStoreError c _ err -> atomically $ sendError ec [c] err
    PEViewStored c v -> viewStored chan rp c v
    PEViewStoreError c _ err -> atomically $ sendError ec [c] err
    PEProjectStored c p -> atomically $ projectStored rp c p
    PEProjectStoreError c _ err -> atomically $ sendError ec [c] err
    PEViewForProjectLoaded c v -> viewForProjectLoaded chan rp c v
    PEViewForProjectLoadError c _ err -> atomically $ sendError ec [c] err
    PECalculationLoaded c cc -> atomically $ calculationLoaded rp c cc
    PECalculationLoadError c _ err -> atomically $ sendError ec [c] err
    where ec = evtChan rp



viewForProjectLoaded :: ProjectChan -> RuntimeProject -> WAClient -> View -> IO ()
viewForProjectLoaded chan rp c v = do
     p <- atomically $ readTVar (project rp)
     vChan <- viewToChan (evtChan rp) (projectName p) v
     atomically $ do modifyTVar (viewChanByName rp) (M.insert (viewName v) vChan)
                     sendSubscriptionToView vChan c
                     sendDependedMapsToVIew chan rp c v

viewLoaded :: WAClient -> RuntimeProject -> View -> STM ()
viewLoaded c rp v = do
    pn <- prjName rp
    writeTChan (evtChan rp) (EMWebEvent [c] $ WEViewLoaded pn v)

mapsLoaded :: RuntimeProject -> WAClient -> [XNamedMap] -> STM ()
mapsLoaded rp c ms = do
     pn <- prjName rp
     writeTChan (evtChan rp) (EMWebEvent [c] $ WEMapsLoaded pn ms)

mapsForViewLoaded :: RuntimeProject -> WAClient -> ViewName -> [XNamedMap] -> STM ()
mapsForViewLoaded rp c vn ms = do
    vs <- readTVar $ viewChanByName rp
    pn <- prjName rp
    case M.lookup vn vs of
        Just vChan -> writeTChan vChan (VMMaps ms)
        Nothing -> sendStringError (evtChan rp) [c] ("view " ++ show vn ++ " to add the maps to not found in project " ++ show pn)

mapStored :: ProjectChan -> RuntimeProject -> WAClient -> XNamedMap -> STM ()
mapStored chan rp c m = do
    let mn = xmapName m
    p <- readTVar (project rp)
    let pn = projectName p
    cbm <- readTVar $ calculationChanByMap rp
    mapM_ (flip sendToAll (CMMap m) ) (M.lookup mn cbm)
    storeProject chan rp c p
    writeTChan (evtChan rp) (EMWebEvent [c] $ WEMapStored pn mn)

calculationStored :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> IO ()
calculationStored chan rp c cc = do
    p <- readTVarIO $ project rp
    if elem (calculationName cc) (calculations p)
        then addCalculation rp cc
        else atomically $ updateCalculation rp c p cc
    atomically $ do
        storeProject chan rp c p
        sendInfo (evtChan rp) [c] ("The calculation " ++ show (calculationName cc) ++ " has been stored")

calculationLoaded :: RuntimeProject -> WAClient -> Calculation -> STM ()
calculationLoaded rp c cc = do
    let cs = CalculationSource {
        sourceCalculationName = calculationName cc,
        sourceResultName = resultName cc,
        formulaText = formulaToText (formula cc),
        sourceOperationMode = operationMode cc
    }
    pn <- prjName rp
    writeTChan (evtChan rp) (EMWebEvent [c] $ WECalculationLoaded pn cs)


viewStored :: ProjectChan -> RuntimeProject -> WAClient -> View -> IO ()
viewStored chan rp c v = do
    p <- readTVarIO $ project rp
    if elem (viewName v) (views p)
        then atomically $ updateView rp c p v
        else addView rp v
    atomically $ do
        storeProject chan rp c p
        sendInfo (evtChan rp) [c] ("The view " ++ show (viewName v) ++ " has been stored")

projectStored :: RuntimeProject -> WAClient -> Project -> STM()
projectStored rp c p = do
    writeTVar (project rp) p
    writeTChan (evtChan rp) (EMWebEvent [c] $ WEProjectStored (projectName p))

sendDependedMapsToVIew :: ProjectChan -> RuntimeProject -> WAClient -> View -> STM ()
sendDependedMapsToVIew chan rp c v = do
     p <- readTVar $ project rp
     case L.find (\s -> sourceType s == FileSource) (sources p) of
        Just fileSources -> do
             let toLoad = L.intersect (viewDependencies v) (sourceOfMaps fileSources)
             writeTChan (loadChan $ chans rp)  $ LMLoadMapsForView chan c (projectName p) (viewName v) toLoad
        Nothing -> return ()


sendSubscriptionToView :: ViewChan -> WAClient -> STM ()
sendSubscriptionToView vchan c = writeTChan vchan (VMSubscribeToView c)

addCalculation :: RuntimeProject -> Calculation -> IO()
addCalculation rp cc = do
    let cn = calculationName cc
    atomically $ modifyTVar (project rp) (\p -> p { calculations = cn : (calculations p)} )
    cch <- calculationToChan cc
    atomically $ modifyTVar (calculationChanByName rp)  (M.insert cn cch)
    atomically $ writeTChan cch (CMUpdateCalculation cc)

updateCalculation :: RuntimeProject -> WAClient -> Project -> Calculation -> STM()
updateCalculation rp c p cc = do
    let cn = calculationName cc
    cbn <- readTVar $ calculationChanByName rp
    case M.lookup cn cbn of
        Just cch -> writeTChan cch (CMUpdateCalculation cc)
        Nothing -> sendStringError (evtChan rp) [c] ("stored calculation " ++ show cn ++ " not found in project " ++ show (projectName p))


addView :: RuntimeProject -> View -> IO()
addView rp v = do
    let vn = viewName v
    atomically $ modifyTVar (project rp) (\p -> p { views = vn : views p })
    pn <- atomically $ prjName rp
    vch <- viewToChan (evtChan rp) pn v
    atomically $ modifyTVar (viewChanByName rp) (\vbn  -> M.insert vn vch vbn )
    atomically $ writeTChan vch (VMUpdate v)

updateView :: RuntimeProject -> WAClient -> Project -> View -> STM()
updateView rp c p v = do
    let vn = viewName v
    vbn <- readTVar $ viewChanByName rp
    case M.lookup vn vbn of
        Just vch -> writeTChan vch (VMUpdate v)
        Nothing -> sendStringError (evtChan rp) [c] ("stored view " ++ show vn ++ " not found in project " ++ show (projectName p))


storeProject :: ProjectChan -> RuntimeProject -> WAClient -> Project -> STM ()
storeProject chan rp c p = do
    writeTChan (storeChan $ chans rp) (StMStoreExistingProject chan c p)