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
                    p <- readTVar $ project rp
                    cbm <- readTVar $ calculationChanByMap rp
                    cbr <- readTVar $ calculationByResult rp
                    cbn <- readTVar $ calculationChanByName rp
                    let mp = p { calculations = cn : calculations p}
                    let mcbn = M.insert cn cch cbn
                    let mcbr = M.insert (resultName cc) cn cbr
                    let mcbm = introduceChanToMap cch deps cbm
                    writeTVar (project rp) mp
                    writeTVar (calculationChanByName rp) mcbn
                    writeTVar (calculationByResult rp) mcbr
                    writeTVar (calculationChanByMap rp) mcbm
                    writeTChan (ccChannel cch) (CMUpdateCalculation cc)
                    sendDependedMapsToCalculation chan mp (chans rp) c cc
                    sendAllCalculationsToNotify c cc mcbr mcbn mcbm (evtChan rp)
    where logDbg t = logDebug (logChan $ chans rp) "project" t


updateCalculation :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> STM()
updateCalculation chan rp c cc = do
    let cn = calculationName cc
    cbn <- readTVar $ calculationChanByName rp
    case M.lookup cn cbn of
        Just cch -> updateFoundCalculation chan rp c cc cch
        Nothing -> do
            pn <- prjName rp
            sendStringError (evtChan rp) [c] ("stored calculation " ++ show cn ++ " not found in project " ++ show pn)

updateFoundCalculation :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> CalculationChan -> STM()
updateFoundCalculation chan rp c cc cch =  do
    modifyTVar (calculationChanByMap rp) $ rebuildCalculationChanByMapForChan cch depsMaps
    writeTChan (ccChannel cch) (CMUpdateCalculation cc)
    p <- readTVar $ project rp
    sendDependedMapsToCalculation chan p (chans rp) c cc
    cbm <- readTVar $ calculationChanByMap rp
    cbr <- readTVar $ calculationByResult rp
    cbn <- readTVar $ calculationChanByName rp
    sendAllCalculationsToNotify c cc cbr cbn cbm (evtChan rp)
    where depsMaps = calculationDependenciesMaps cc

sendAllCalculationsToNotify :: WAClient -> Calculation -> CalculationNameByResult -> CalculationChanByName -> CalculationChanByMap-> EventChan -> STM()
sendAllCalculationsToNotify c cc cbr cbn cbm ech =
  case dependenciesCalculations cbr cbn cc of
    Right crs -> mapM_ sendCalculationsToNotify crs
    Left err -> sendError ech [c] err
  where calculationsToNotify rch = M.lookup (rccResult rch) cbm
        sendCalculationsToNotify ::  CalculationResultWithChan -> STM ()
        sendCalculationsToNotify rch = case calculationsToNotify rch of
            Just cs ->  writeTChan (ccChannel $ rccChan rch) (CMUpdateCalculationsToNotify cs)
            Nothing -> sendStringError ech [c] $ "No calculation found for " ++ (show $ rccResult rch)

dependenciesCalculations :: CalculationNameByResult -> CalculationChanByName -> Calculation ->  Either Error [CalculationResultWithChan]
dependenciesCalculations cbr cbn cc =
    CM.sequence $ map resultNameToResultChan dependenciesCalcResultNames
    where depsMaps = calculationDependenciesMaps cc
          dependenciesCalcResultNames =   map (\(r, n) -> CalculationResultWithName { crnResult = r, name = n }) $ M.toList (M.restrictKeys cbr (S.fromList depsMaps))
          resultNameToResultChan rn = case M.lookup (name rn) cbn of
                                      Just ch -> Right CalculationResultWithChan { rccResult = crnResult rn, rccChan = ch }
                                      _ -> Left $ mkError ("No calculation with name " ++ (show $ name rn) ++ "found")

mapsForCalculationLoaded :: RuntimeProject -> WAClient -> CalculationName -> [XNamedMap] -> STM ()
mapsForCalculationLoaded rp c cn ms = do
    cbn <- readTVar $ calculationChanByName rp
    pn <- prjName rp
    case M.lookup cn cbn of
        Just cChan -> writeTChan (ccChannel cChan) (CMMaps ms)
        Nothing -> sendStringError (evtChan rp) [c] ("calculation " ++ show cn ++ " to add the maps to not found in project " ++ show pn)

mapsForCalculationsLoaded :: RuntimeProject -> WAClient -> [XNamedMap] -> STM ()
mapsForCalculationsLoaded rp _ ms = do
    cbm <- readTVar $ calculationChanByMap rp
    mapM_ (findCalculationsAndSendMap rp cbm) ms

findCalculationsAndSendMap :: RuntimeProject -> CalculationChanByMap -> XNamedMap -> STM ()
findCalculationsAndSendMap rp cbm m = do
    let mn = mapName m
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

calculationUpdated :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> IO ()
calculationUpdated chan rp c cc = do
    p <- readTVarIO $ project rp
    atomically $ do
        updateCalculation chan rp c cc
        storeProject chan rp c
        writeTChan (evtChan rp) (EMWebEvent [c] $ WECalculationUpdated (projectName p) (calculationName cc))

calculationAdded :: ProjectChan -> RuntimeProject -> WAClient -> Calculation -> IO ()
calculationAdded chan rp c cc = do
    p <- readTVarIO $ project rp
    addCalculation chan rp c cc
    atomically $ do
        storeProject chan rp c
        writeTChan (evtChan rp) (EMWebEvent [c] $ WECalculationAdded (projectName p) (calculationName cc))

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

sendDependedMapsToCalculation :: ProjectChan -> Project -> CommonChans -> WAClient -> Calculation -> STM ()
sendDependedMapsToCalculation chan p chs c cc = do
     case sourcesOfTypeInProject FileSource p of
        Just fileSources -> do
             let toLoad = L.intersect (calculationDependenciesMaps cc) fileSources
             writeTChan (loadChan chs) $ LMLoadMapsForCalculation chan c (projectName p) (calculationName cc) toLoad
        Nothing -> return ()

storeProject :: ProjectChan -> RuntimeProject -> WAClient -> STM ()
storeProject chan rp c = do
    p <- readTVar $ project rp
    writeTChan (storeChan $ chans rp) (StMStoreExistingProject chan c p)

