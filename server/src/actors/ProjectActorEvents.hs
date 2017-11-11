{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProjectActorEvents where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as B
import Debug.Trace (trace)

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
    PEViewLoaded c v -> atomically $ viewForClientLoaded c rp v
    PEViewLoadError c _ err -> atomically $ sendError ec [c] err
    PEMapsLoaded c ms -> atomically $ mapsLoaded rp c ms
    PEMapsLoadError c _ err -> atomically $ sendError ec [c] err
    PEMapsForViewLoaded c vn ms -> atomically $ mapsForViewLoaded rp c vn ms
    PEMapsForViewLoadError c _ _ err -> atomically $ sendError ec [c] err
    PEMapsForCalculationsLoaded c ms -> atomically $ mapsForCalculationsLoaded rp c ms
    PEMapsForCalculationsLoadError c  _ err -> atomically $ sendError ec [c] err
    PEMapStored c m -> atomically $ mapStored chan rp c m
    PEMapStoreError c _ err -> atomically $ sendError ec [c] err
    PECalculationStored c cc -> calculationStored chan rp c cc
    PECalculationStoreError c _ err -> atomically $ sendError ec [c] err
    PEViewStored c v -> viewStored chan rp c v
    PEViewStoreError c _ err -> atomically $ sendError ec [c] err
    PEProjectStored _ p -> atomically $ projectStored rp p
    PEProjectStoreError c _ err -> atomically $ sendError ec [c] err
    PEViewForProjectLoaded c v -> viewForProjectLoaded chan rp c v
    PEViewForProjectLoadError c _ err -> atomically $ sendError ec [c] err
    PECalculationLoaded c cc -> atomically $ calculationLoaded rp c cc
    PECalculationLoadError c _ err -> atomically $ sendError ec [c] err
    where ec = evtChan rp


viewForClientLoaded :: WAClient -> RuntimeProject -> View -> STM ()
viewForClientLoaded c rp v = do
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

mapsForCalculationsLoaded :: RuntimeProject -> WAClient -> [XNamedMap] -> STM ()
mapsForCalculationsLoaded rp _ ms = do
    cbm <- readTVar $ calculationChanByMap rp
    mapM_ (findAndSend cbm) ms
    where findAndSend cbm m = do
            let mn = xmapName m
            mapM_ (sendMapToCalculations m) (M.lookup mn cbm)
          sendMapToCalculations m = mapM_ (\c -> writeTChan c (CMMap (traceMap m)))
          traceMap :: XNamedMap -> XNamedMap
          traceMap m = trace ("sending map " ++ show (xmapName m) ++ " to calculations") m

newSource :: SourceType -> [XMapName] -> Source
newSource st mns = Source { sourceType = st, sourceOfMaps = mns }

mapStored :: ProjectChan -> RuntimeProject -> WAClient -> XNamedMap -> STM ()
mapStored chan rp c m = do
        let mn = xmapName m
        modifyTVar (project rp) (updateProjectWithFileMap mn)
        p <- readTVar (project rp)
        storeProject chan rp c
        sendToAllCalculations mn
        sendToAllViews mn
        let pn = projectName p
        writeTChan (evtChan rp) (EMWebEvent [c] $ WEMapStored pn mn)
        where sendToAllCalculations mn = do
                    cbm <- readTVar $ calculationChanByMap rp
                    mapM_ (flip sendToAll (CMMap m) ) (M.lookup mn cbm)
              sendToAllViews mn = do
                    vbm <- readTVar $ viewChanByMap rp
                    mapM_ (flip sendToAll (VMMaps [m]) ) (M.lookup mn vbm)


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
        storeProject chan rp c
        sendInfo (evtChan rp) [c] ("The view " ++ show (viewName v) ++ " has been stored")


calculationStored :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> IO ()
calculationStored chan rp c cc = do
    p <- readTVarIO $ project rp
    if elem (calculationName cc) (calculations p)
        then atomically $ updateCalculation rp c cc
        else addCalculation rp cc
    atomically $ do
        storeProject chan rp c
        sendInfo (evtChan rp) [c] ("The calculation " ++ show (calculationName cc) ++ " has been stored")

projectStored :: RuntimeProject -> Project -> STM()
projectStored rp p = do
    writeTVar (project rp) p
    cs <- readTVar $ subscribedClients rp
    writeTChan (evtChan rp) (EMWebEvent cs $ WEProjectStored p)

sendDependedMapsToView :: ProjectChan -> RuntimeProject -> WAClient -> View -> STM ()
sendDependedMapsToView chan rp c v = do
     p <- readTVar $ project rp
     case L.find (\s -> sourceType s == FileSource) (sources p) of
        Just fileSources -> do
             let toLoad = L.intersect (viewDependencies v) (sourceOfMaps fileSources)
             writeTChan (loadChan $ chans rp)  $ LMLoadMapsForView chan c (projectName p) (viewName v) toLoad
        Nothing -> return ()

updateProjectWithFileMap :: XMapName -> Project -> Project
updateProjectWithFileMap mn p = p { sources = updateFileSources : notFileSources }
    where isFileSources s = sourceType s == FileSource
          notFileSources = filter (not . isFileSources) (sources p)
          updateFileSources = B.maybe (newSource FileSource [mn]) updateFileSourcesContent findFileSources
          findFileSources = L.find isFileSources (sources p)
          updateFileSourcesContent s = s { sourceOfMaps = updateMaps (sourceOfMaps s)}
          updateMaps mns = L.union mns [mn]

sendSubscriptionToView :: ViewChan -> WAClient -> STM ()
sendSubscriptionToView vchan c = writeTChan vchan (VMSubscribeToView c)

addCalculation :: RuntimeProject -> Calculation -> IO()
addCalculation rp cc = do
    cch <- calculationToChan (logChan (chans rp)) cc
    atomically $ do
                    let cn = calculationName cc
                    modifyTVar (project rp) (\p -> p { calculations = cn : calculations p} )
                    modifyTVar (calculationChanByName rp)  $ M.insert cn cch
                    writeTChan cch (CMUpdateCalculation cc)

updateCalculation :: RuntimeProject -> WAClient -> Calculation -> STM()
updateCalculation rp c cc = do
    let cn = calculationName cc
    cbn <- readTVar $ calculationChanByName rp
    pn <- prjName rp
    case M.lookup cn cbn of
        Just cch -> writeTChan cch (CMUpdateCalculation cc)
        Nothing -> sendStringError (evtChan rp) [c] ("stored calculation " ++ show cn ++ " not found in project " ++ show pn)

viewForProjectLoaded :: ProjectChan -> RuntimeProject -> WAClient -> View -> IO ()
viewForProjectLoaded chan rp c v = do
     p <- readTVarIO $ project rp
     vch <- viewToChan (evtChan rp) (projectName p) v
     atomically $ do addViewToProject rp vch v
                     sendSubscriptionToView vch c
                     sendDependedMapsToView chan rp c v
                     listenToDependentCalculations rp vch v

addViewToProject ::  RuntimeProject -> ViewChan -> View -> STM ()
addViewToProject rp vch v = do
    modifyTVar (viewChanByName rp) $ M.insert (viewName v) vch
    modifyTVar (viewChanByMap rp) $ updateViewChanByMap vch (viewDependencies v)

addView :: RuntimeProject -> View -> IO()
addView rp v = do
    let vn = viewName v
    atomically $ modifyTVar (project rp) (\p -> p { views = vn : views p })
    pn <- atomically $ prjName rp
    vch <- viewToChan (evtChan rp) pn v
    atomically $ do
        addViewToProject rp vch v
        writeTChan vch (VMUpdate v)

updateViewChanByMap :: ViewChan -> [XMapName] -> ViewChanByMap -> ViewChanByMap
updateViewChanByMap vch mns = trace ("updateView:" ++ show mns) $ updated . cleaned
    where cleaned :: ViewChanByMap -> ViewChanByMap
          cleaned = M.map (filter (/= vch))
          updated :: ViewChanByMap -> ViewChanByMap
          updated vbm = foldr (\mn cvbm-> M.insertWith (++) mn [vch] cvbm) vbm mns

updateView :: RuntimeProject -> WAClient -> Project -> View -> STM()
updateView rp c p v = do
    let vn = viewName v
    vbn <- readTVar $ viewChanByName rp
    case M.lookup vn vbn of
        Just vch -> writeTChan vch (VMUpdate v)
        Nothing -> sendStringError (evtChan rp) [c] ("stored view " ++ show vn ++ " not found in project " ++ show (projectName p))

storeProject :: ProjectChan -> RuntimeProject -> WAClient -> STM ()
storeProject chan rp c = do
    p <- readTVar $ project rp
    writeTChan (storeChan $ chans rp) (StMStoreExistingProject chan c p)

listenToDependentCalculations :: RuntimeProject -> ViewChan -> View -> STM ()
listenToDependentCalculations rp vch v = do
    let deps = viewDependencies v
    cbr <- readTVar $ calculationByResult rp
    let cs = B.mapMaybe (\d -> M.lookup d cbr) deps
    cbn <- readTVar $ calculationChanByName rp
    let chs = B.mapMaybe (\c -> M.lookup c cbn) cs
    mapM_ (\ch -> writeTChan ch (CMViewStarted vch)) chs
