module ProjectActorEventsForCalculation where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Debug.Trace (trace)

import ProjectState
import ProjectMessages
import EventMessages
import WebMessages
import LoadMessages
import StoreMessages
import CommonChannels
import CalculationMessages
import Dependencies
import WebClients
import XMapTypes
import Project
import Calculation
import CalculationBuild
import FormulaText

addCalculation :: RuntimeProject -> Calculation -> IO()
addCalculation rp cc = do
    cch <- calculationToChan (logChan (chans rp)) cc
    atomically $ do
                    let cn = calculationName cc
                    modifyTVar (project rp) (\p -> p { calculations = cn : calculations p} )
                    modifyTVar (calculationChanByName rp)  $ M.insert cn cch
                    modifyTVar (calculationByResult rp) $ M.insert (resultName cc) cn
                    writeTChan cch (CMUpdateCalculation cc)

updateCalculation :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> STM()
updateCalculation chan rp c cc = do
    let cn = calculationName cc
    cbn <- readTVar $ calculationChanByName rp
    pn <- prjName rp
    case M.lookup cn cbn of
        Just cch -> updateFoundCalculation cch
        Nothing -> sendStringError (evtChan rp) [c] ("stored calculation " ++ show cn ++ " not found in project " ++ show pn)
    where updateFoundCalculation cch =  do
                                       modifyTVar (calculationChanByMap rp) $ updateCalculationChanByMap cch (calculationDependencies cc)
                                       writeTChan cch (CMUpdateCalculation cc)
                                       sendDependedMapsToCalculation chan rp c cc

mapsForCalculationLoaded :: RuntimeProject -> WAClient -> CalculationName -> [XNamedMap] -> STM ()
mapsForCalculationLoaded rp c cn ms = do
    cs <- readTVar $ calculationChanByName rp
    pn <- prjName rp
    case M.lookup cn cs of
        Just cChan -> writeTChan cChan (CMMaps ms)
        Nothing -> sendStringError (evtChan rp) [c] ("calculation " ++ show cn ++ " to add the maps to not found in project " ++ show pn)

mapsForCalculationsLoaded :: RuntimeProject -> WAClient -> [XNamedMap] -> STM ()
mapsForCalculationsLoaded rp _ ms = do
    cbm <- readTVar $ calculationChanByMap rp
    mapM_ (findAndSend cbm) ms
    where findAndSend cbm m = do
            let mn = xmapName m
            mapM_ (sendMapToCalculations m) (M.lookup mn cbm)
          sendMapToCalculations m = mapM_ (\c -> writeTChan c (CMMaps [traceMap m]))
          traceMap :: XNamedMap -> XNamedMap
          traceMap m = trace ("sending map " ++ show (xmapName m) ++ " to calculations") m

calculationForClientLoaded :: RuntimeProject -> WAClient -> Calculation -> STM ()
calculationForClientLoaded rp c cc = do
    let cs = CalculationSource {
        sourceCalculationName = calculationName cc,
        sourceResultName = resultName cc,
        formulaText = formulaToText (formula cc),
        sourceOperationMode = operationMode cc
    }
    pn <- prjName rp
    writeTChan (evtChan rp) (EMWebEvent [c] $ WECalculationLoaded pn cs)

calculationStored :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> IO ()
calculationStored chan rp c cc = do
    p <- readTVarIO $ project rp
    if elem (calculationName cc) (calculations p)
        then atomically $ updateCalculation chan rp c cc
        else addCalculation rp cc
    atomically $ do
        storeProject chan rp c
        sendInfo (evtChan rp) [c] ("The calculation " ++ show (calculationName cc) ++ " has been stored")

updateCalculationChanByMap :: CalculationChan -> [XMapName] -> CalculationChanByMap -> CalculationChanByMap
updateCalculationChanByMap cch mns = trace ("updateCalculation:" ++ show mns) $ updated . cleaned
    where cleaned :: CalculationChanByMap -> CalculationChanByMap
          cleaned = M.map (filter (/= cch))
          updated :: CalculationChanByMap -> CalculationChanByMap
          updated cbm = foldr addToMultimap cbm mns
          addToMultimap :: XMapName -> CalculationChanByMap -> CalculationChanByMap
          addToMultimap mn = M.insertWith (++) mn [cch]

sendDependedMapsToCalculation :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> STM ()
sendDependedMapsToCalculation chan rp c cc = do
     p <- readTVar $ project rp
     case sourcesOfTypeInProject FileSource p of
        Just fileSources -> do
             let toLoad = L.intersect (calculationDependencies cc) fileSources
             writeTChan (loadChan $ chans rp) $ LMLoadMapsForCalculation chan c (projectName p) (calculationName cc) toLoad
        Nothing -> return ()

storeProject :: ProjectChan -> RuntimeProject -> WAClient -> STM ()
storeProject chan rp c = do
    p <- readTVar $ project rp
    writeTChan (storeChan $ chans rp) (StMStoreExistingProject chan c p)

