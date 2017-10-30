{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProjectActorRequests where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.List as L
import qualified Data.Map.Strict as M

import ProjectState
import ProjectMessages
import ViewMessages
import EventMessages
import WebMessages
import LoadMessages
import StoreMessages
import CommonChannels
import WebClients
import XMapTypes
import View
import Project
import Calculation
import FormulaParser

handleRequests:: ProjectChan -> RuntimeProject -> ProjectRequest -> STM ()
handleRequests chan rp r= case r of
    PRSubscribeToProject c -> subscribeToProject c rp
    PRUnsubscribeFromProject c -> removeSubscriber c rp
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
    PRStartCalculations c -> startCalculations chan rp c

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
    addSubscriber rp c
    writeTChan (evtChan rp) $ EMWebEvent [c] (WEProjectContent p)

removeSubscriber :: WAClient -> RuntimeProject -> STM()
removeSubscriber c rp = modifyTVar (subscribedClients rp) (filter notSameClient)
    where notSameClient ci = ci /= c

mapsInProject :: RuntimeProject -> WAClient -> STM ()
mapsInProject rp c = do
    p <- readTVar (project rp)
    cbm <- readTVar $ calculationChanByMap rp
    cbr <- readTVar $ calculationByResult rp
    let ss = L.nub (concatMap sourceOfMaps (sources p) ++ M.keys cbm ++ M.keys cbr)
    writeTChan (evtChan rp) $ EMWebEvent [c] (WEMapsInProject (projectName p) ss)

subscribeToView :: ProjectChan -> RuntimeProject -> WAClient -> ViewName -> STM ()
subscribeToView chan rp c vn   = do
    vs <- readTVar $ viewChanByName rp
    pn <- prjName rp
    case M.lookup vn vs of
        Just vChan -> writeTChan vChan (VMSubscribeToView c)
        Nothing -> writeTChan (loadChan $ chans rp) (LMLoadViewForProject chan c pn vn)

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

loadView :: ProjectChan -> RuntimeProject -> WAClient -> ViewName -> STM ()
loadView chan rp c vn = do
    pn <- prjName rp
    writeTChan (loadChan $ chans rp) $ LMLoadView chan c pn vn

startCalculations :: ProjectChan -> RuntimeProject -> WAClient -> STM()
startCalculations chan rp c = do
    p <- readTVar $ project rp
    case L.find (\s -> sourceType s == FileSource) (sources p) of
        Just fileSources -> do
            cbm <- readTVar (calculationChanByMap rp)
            let cms = L.intersect (M.keys cbm) (sourceOfMaps fileSources)
            writeTChan (loadChan $ chans rp) $ LMLoadMapsForCalculations chan c (projectName p) cms
        Nothing -> return ()

addSubscriber ::RuntimeProject -> WAClient -> STM ()
addSubscriber rp c= modifyTVar (subscribedClients rp) (\cs -> c : cs)
