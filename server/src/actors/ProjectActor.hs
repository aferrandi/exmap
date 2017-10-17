{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProjectActor (actorProject) where

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
import FormulaParser
import FormulaText

actorProject :: ProjectChan -> RuntimeProject -> IO ()
actorProject chan rp = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                PMRequest r -> do
                    print $ "handling project request " ++ show r
                    atomically $ handleRequests chan rp r
                    loop
                PMEvent e -> do
                    print $ "handling project event " ++ show e
                    handleEvent chan rp e
                    loop
                PMStop -> return ()

handleRequests:: ProjectChan -> RuntimeProject -> ProjectRequest -> STM ()
handleRequests chan rp r= case r of
    PRSubscribeToProject c -> subscribeToProject c rp
    PRUnsubscribeFromProject c -> unsubscribeFromProject c rp
    PRSubscribeToView c vn -> subscribeToView chan rp c vn
    PRUnsubscribeFromView c vn -> unsubscribeFromView c vn rp
    PRMapsInProject c -> mapsInProject rp c
    PRUpdateProject c p -> updateProject chan rp c p
    PRLoadMaps c mns -> loadMaps c chan rp mns
    PRStoreMap c m -> storeMap c chan rp m
    PRLoadCalculation c cn -> loadCalculation chan rp c cn
    PRStoreCalculation c cs-> storeCalculation chan rp c cs
    PRLoadView c vn -> loadView chan rp c vn
    PRStoreView c v -> storeView chan rp c v

handleEvent :: ProjectChan -> RuntimeProject -> ProjectEvent -> IO ()
handleEvent chan rp e = case e of
    PEViewLoaded c v -> atomically $ viewLoaded c rp v
    PEViewLoadError c _ err -> atomically $ sendError ec [c] err
    PEMapsLoaded c ms -> atomically $ mapsLoaded rp c ms
    PEMapsLoadError c _ err -> atomically $ sendError ec [c] err
    PEMapsForViewLoaded c vn ms -> atomically $ mapsForViewLoaded rp c vn ms
    PEMapsForViewLoadError c _ _ err -> atomically $ sendError ec [c] err
    PEMapStored c m -> atomically $ mapStored rp c m
    PEMapStoreError c _ err -> atomically $ sendError ec [c] err
    PECalculationStored c cc -> calculationStored rp c cc
    PECalculationStoreError c _ err -> atomically $ sendError ec [c] err
    PEViewStored c v -> viewStored rp c v
    PEViewStoreError c _ err -> atomically $ sendError ec [c] err
    PEProjectStored c p -> atomically $ projectStored rp c p
    PEProjectStoreError c _ err -> atomically $ sendError ec [c] err
    PEViewForProjectLoaded c v -> viewForProjectLoaded c chan rp v
    PEViewForProjectLoadError c _ err -> atomically $ sendError ec [c] err
    PECalculationLoaded c cc -> atomically $ calculationLoaded rp c cc
    PECalculationLoadError c _ err -> atomically $ sendError ec [c] err
    where ec = evtChan rp

loadMaps :: WAClient -> ProjectChan -> RuntimeProject -> [XMapName] -> STM ()
loadMaps c chan rp mns = do
     pn <- prjName rp
     writeTChan (loadChan $ chans rp)  $ LMLoadMaps chan c pn mns

storeMap :: WAClient -> ProjectChan -> RuntimeProject -> XNamedMap -> STM ()
storeMap c chan rp m = do
    pn <- prjName rp
    writeTChan (storeChan $ chans rp)  $ StMStoreMap chan c pn  m

subscribeToProject :: WAClient -> RuntimeProject -> STM()
subscribeToProject c rp = do
    p <- readTVar $ project rp
    modifyTVar (subscribedClients rp) (\cs -> c : cs)
    writeTChan (evtChan rp) (EMWebEvent [c] (WEProjectContent p))

unsubscribeFromProject :: WAClient -> RuntimeProject -> STM()
unsubscribeFromProject c rp = modifyTVar (subscribedClients rp) (filter notSameClient)
    where notSameClient ci = ci /= c

viewForProjectLoaded :: WAClient -> ProjectChan -> RuntimeProject -> View -> IO ()
viewForProjectLoaded c chan rp v = do
     p <- atomically $ readTVar (project rp)
     vChan <- viewToChan (evtChan rp) (projectName p) v
     atomically $ do modifyTVar (viewChanByName rp) (M.insert (viewName v) vChan)
                     sendSubscriptionToView vChan c
                     sendDependedMapsToVIew chan rp c v

viewLoaded :: WAClient -> RuntimeProject -> View -> STM ()
viewLoaded c rp v = do
    pn <- prjName rp
    writeTChan (evtChan rp) (EMWebEvent [c] $ WEViewLoaded pn v)



sendDependedMapsToVIew :: ProjectChan -> RuntimeProject -> WAClient -> View -> STM ()
sendDependedMapsToVIew chan rp c v = do
     p <- readTVar $ project rp
     case L.find (\s -> sourceType s == FileSource) (sources p) of
        Just fileSources -> do
             let toLoad = L.intersect (viewDependencies v) (sourceOfMaps fileSources)
             writeTChan (loadChan $ chans rp)  $ LMLoadMapsForView chan c (projectName p) (viewName v) toLoad
        Nothing -> return ()

loadCalculation :: ProjectChan -> RuntimeProject -> WAClient -> CalculationName -> STM ()
loadCalculation chan rp c cn = do
     pn <- prjName rp
     writeTChan (loadChan $ chans rp)  $ LMLoadCalculation chan c pn cn


storeCalculation :: ProjectChan -> RuntimeProject -> WAClient -> CalculationSource -> STM ()
storeCalculation chan rp c cs = do
    pn <- prjName rp
    let ft = formulaText cs
    case parseFormula ft of
        Left err -> sendStringError  (evtChan rp) [c] ("Parsing the formula " ++ show ft ++ " got " ++ show err)
        Right f -> do
                let cc = Calculation {
                        calculationName = sourceCalculationName cs,
                        resultName = sourceResultName cs,
                        formula = f,
                        operationMode = sourceOperationMode cs
                }
                writeTChan (storeChan $ chans rp)  $ StMStoreCalculation chan c pn cc

storeView :: ProjectChan -> RuntimeProject -> WAClient -> View -> STM ()
storeView chan rp c v = do
    pn <- prjName rp
    writeTChan (storeChan $ chans rp)  $ StMStoreView chan c pn v

sendSubscriptionToView :: ViewChan -> WAClient -> STM ()
sendSubscriptionToView vchan c = writeTChan vchan (VMSubscribeToView c)

subscribeToView :: ProjectChan -> RuntimeProject -> WAClient -> ViewName -> STM ()
subscribeToView chan rp c vn   = do
    vs <- readTVar $ viewChanByName rp
    pn <- prjName rp
    case M.lookup vn vs of
        Just vChan -> sendSubscriptionToView vChan c
        Nothing -> writeTChan (loadChan $ chans rp) (LMLoadViewForProject chan c pn vn)

loadView :: ProjectChan -> RuntimeProject -> WAClient -> ViewName -> STM ()
loadView chan rp c vn = do
    pn <- prjName rp
    writeTChan (loadChan $ chans rp) (LMLoadView chan c pn vn)

unsubscribeFromView :: WAClient -> ViewName -> RuntimeProject -> STM ()
unsubscribeFromView c vn rp = do
    vs <- readTVar $ viewChanByName rp
    pn <- prjName rp
    case M.lookup vn vs of
        Just vChan -> writeTChan vChan (VMUnsubscribeFromView c)
        Nothing -> sendStringError  (evtChan rp) [c] ("view " ++ show vn ++ " to unsubscribe from not found in project " ++ show pn)

updateProject :: ProjectChan -> RuntimeProject -> WAClient -> Project -> STM ()
updateProject chan rp c p = do
    writeTChan (storeChan $ chans rp) (StMStoreExistingProject chan c p)
    -- update calculations and views

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

mapStored :: RuntimeProject -> WAClient -> XNamedMap -> STM ()
mapStored rp c m = do
    let mn = xmapName m
    pn <- prjName rp
    cbm <- readTVar $ calculationChanByMap rp
    mapM_ (flip sendToAll (CMMap m) ) (M.lookup mn cbm)
    writeTChan (evtChan rp) (EMWebEvent [c] $ WEMapStored pn mn)

calculationStored :: RuntimeProject -> WAClient -> Calculation -> IO ()
calculationStored rp c cc = do
    p <- readTVarIO $ project rp
    if elem (calculationName cc) (calculations p)
        then addCalculation rp cc
        else atomically $ updateCalculation rp c p cc
    atomically $ sendInfo (evtChan rp) [c] ("The calculation " ++ show (calculationName cc) ++ " has been stored")

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


viewStored :: RuntimeProject -> WAClient -> View -> IO ()
viewStored rp c v = do
    p <- readTVarIO $ project rp
    if elem (viewName v) (views p)
        then atomically $ updateView rp c p v
        else addView rp v
    atomically $ sendInfo (evtChan rp) [c] ("The view " ++ show (viewName v) ++ " has been stored")

projectStored :: RuntimeProject -> WAClient -> Project -> STM()
projectStored rp c p = do
    writeTVar (project rp) p
    writeTChan (evtChan rp) (EMWebEvent [c] $ WEProjectStored (projectName p))

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


mapsInProject :: RuntimeProject -> WAClient -> STM ()
mapsInProject rp c = do
    p <- readTVar (project rp)
    cbm <- readTVar $ calculationChanByMap rp
    rbn <- readTVar $ calculationResultByName rp
    let ss = L.nub (concatMap sourceOfMaps (sources p) ++ M.keys cbm ++ M.elems rbn)
    writeTChan (evtChan rp) (EMWebEvent [c] $ WEMapsInProject (projectName p) ss)

evtChan :: RuntimeProject -> EventChan
evtChan rp = eventChan $ chans rp


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

prjName :: RuntimeProject -> STM ProjectName
prjName rp = do
    p <- readTVar (project rp)
    return $ projectName p