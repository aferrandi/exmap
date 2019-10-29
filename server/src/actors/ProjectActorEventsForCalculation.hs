module ProjectActorEventsForCalculation where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad as CM
import Debug.Trace (trace)

import ProjectState
import ProjectMessages
import EventMessages
import WebMessages
import LoadMessages
import LogMessages
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
import Errors



data CalculationResultWithChan = CalculationResultWithChan {
    rccResult :: XMapName,
    rccChan :: CalculationChan
    }

data CalculationResultWithName = CalculationResultWithName {
    crnResult :: XMapName,
    name :: CalculationName
    }

addCalculation :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> IO()
addCalculation chan rp c cc = do
    cbr <- readTVarIO $ calculationByResult rp
    let rs = M.keysSet cbr
    cch <- calculationToChan (logChan (chans rp)) rs cc
    atomically $ do
                    let cn = calculationName cc
                    let deps = calculationDependenciesMaps cc
                    logDbg $ "Maps used by calculation " ++ (show cn) ++ ":" ++ (show deps)
                    modifyTVar (project rp) (\p -> p { calculations = cn : calculations p} )
                    modifyTVar (calculationChanByName rp)  $ M.insert cn cch
                    modifyTVar (calculationByResult rp) $ M.insert (resultName cc) cn
                    modifyTVar (calculationChanByMap rp) $ introduceChanToMap cch deps
                    -- mo need to send CMUpdateCalculationsToNotify since this is a new calculation and no other calculation
                    -- changed the maps by which it is dependent
                    writeTChan (ccChannel cch) (CMUpdateCalculation cc)
                    sendDependedMapsToCalculation chan rp c cc
    where logDbg t = logDebug (logChan $ chans rp) "project" t


updateCalculation :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> STM()
updateCalculation chan rp c cc = do
    let cn = calculationName cc
    cbn <- readTVar $ calculationChanByName rp
    pn <- prjName rp
    case M.lookup cn cbn of
        Just cch -> updateFoundCalculation chan rp c cc cch
        Nothing -> sendStringError (evtChan rp) [c] ("stored calculation " ++ show cn ++ " not found in project " ++ show pn)

updateFoundCalculation :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> CalculationChan -> STM()
updateFoundCalculation chan rp c cc cch =  do
    modifyTVar (calculationChanByMap rp) $ rebuildCalculationChanByMapForChan cch depsMaps
    writeTChan (ccChannel cch) (CMUpdateCalculation cc)
    sendDependedMapsToCalculation chan rp c cc
    cbm <- readTVar $ calculationChanByMap rp
    cbr <- readTVar $ calculationByResult rp
    cbn <- readTVar $ calculationChanByName rp
    sendAllCalculationsToNotify cbr cbn cbm
    where depsMaps = calculationDependenciesMaps cc
          calculationsToNotify cbm rch = M.lookup (rccResult rch) cbm
          sendCalculationsToNotify ::  CalculationChanByMap -> CalculationResultWithChan -> STM ()
          sendCalculationsToNotify cbm rch = case calculationsToNotify cbm rch of
              Just cs ->  writeTChan (ccChannel $ rccChan rch) (CMUpdateCalculationsToNotify cs)
              Nothing -> sendStringError (evtChan rp) [c] $ "No calculation found for " ++ (show $ rccResult rch)
          sendAllCalculationsToNotify cbr cbn cbm = case calculationsWithResultMapsNeededBy cbr cbn cc of
            Right crs -> mapM_ (sendCalculationsToNotify cbm) crs
            Left err -> sendError (evtChan rp) [c] err

calculationsWithResultMapsNeededBy :: CalculationNameByResult -> CalculationChanByName -> Calculation ->  Either Error [CalculationResultWithChan]
calculationsWithResultMapsNeededBy cbr cbn cc =
    CM.sequence $ map buildCalculationResult origins
    where depsMaps = calculationDependenciesMaps cc
          origins =   map (\(r, n) -> CalculationResultWithName { crnResult = r, name = n }) $ M.toList (M.restrictKeys cbr (S.fromList depsMaps))
          buildCalculationResult rn = case M.lookup (name rn) cbn of
                                      Just ch -> Right CalculationResultWithChan { rccResult = crnResult rn, rccChan = ch }
                                      _ -> Left $ mkError ("No calculation with name " ++ (show $ name rn) ++ "found")


mapsForCalculationLoaded :: RuntimeProject -> WAClient -> CalculationName -> [XNamedMap] -> STM ()
mapsForCalculationLoaded rp c cn ms = do
    cs <- readTVar $ calculationChanByName rp
    pn <- prjName rp
    case M.lookup cn cs of
        Just cChan -> writeTChan (ccChannel cChan) (CMMaps ms)
        Nothing -> sendStringError (evtChan rp) [c] ("calculation " ++ show cn ++ " to add the maps to not found in project " ++ show pn)

mapsForCalculationsLoaded :: RuntimeProject -> WAClient -> [XNamedMap] -> STM ()
mapsForCalculationsLoaded rp _ ms = do
    cbm <- readTVar $ calculationChanByMap rp
    mapM_ (findCalculationsAndSendMap rp cbm) ms

findCalculationsAndSendMap :: RuntimeProject -> CalculationChanByMap -> XNamedMap -> STM ()
findCalculationsAndSendMap rp cbm m = do
    let mn = xmapName m
    let cs = M.lookup mn cbm
    logDebug (logChan $ chans rp) "project" $ "sending map " ++ show mn ++ " to " ++ (show $ length cs) ++ " calculations"
    mapM_ (sendMapToCalculations m) cs
    where sendMapToCalculations m ch = mapM_ (\c -> writeTChan (ccChannel c) (CMMaps [m])) ch

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
        else addCalculation chan rp c cc
    atomically $ do
        storeProject chan rp c
        sendInfo (evtChan rp) [c] ("The calculation " ++ show (calculationName cc) ++ " has been stored")

rebuildCalculationChanByMapForChan :: CalculationChan -> [XMapName] -> CalculationChanByMap -> CalculationChanByMap
rebuildCalculationChanByMapForChan cch mns = trace ("updateCalculation:" ++ show mns) $ reintroduceChanToMap . removeChanFromMap
    where removeChanFromMap :: CalculationChanByMap -> CalculationChanByMap
          removeChanFromMap = M.map (filter (/= cch))
          reintroduceChanToMap :: CalculationChanByMap -> CalculationChanByMap
          reintroduceChanToMap ccbm = introduceChanToMap cch mns ccbm

introduceChanToMap :: CalculationChan -> [XMapName] -> CalculationChanByMap -> CalculationChanByMap
introduceChanToMap cch mns ccbm = foldr addToMultimap ccbm mns
    where addToMultimap :: XMapName -> CalculationChanByMap -> CalculationChanByMap
          addToMultimap mn = M.insertWith (++) mn [cch]

sendDependedMapsToCalculation :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> STM ()
sendDependedMapsToCalculation chan rp c cc = do
     p <- readTVar $ project rp
     case sourcesOfTypeInProject FileSource p of
        Just fileSources -> do
             let toLoad = L.intersect (calculationDependenciesMaps cc) fileSources
             writeTChan (loadChan $ chans rp) $ LMLoadMapsForCalculation chan c (projectName p) (calculationName cc) toLoad
        Nothing -> return ()

storeProject :: ProjectChan -> RuntimeProject -> WAClient -> STM ()
storeProject chan rp c = do
    p <- readTVar $ project rp
    writeTChan (storeChan $ chans rp) (StMStoreExistingProject chan c p)

