{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CalculationActor (actorCalculation) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.List as L

import TChans
import XMapTypes
import CalculationState
import Calculation
import ExecFormula
import CalculationMessages
import ViewMessages
import LogMessages
import Errors (mkError)

actorCalculation :: CalculationChan -> RuntimeCalculation -> IO ()
actorCalculation chan rc = loop
    where loop = do
            msg <- atomically $ readTChan chan
            cn <- atomically $ runtimeCalcName rc
            case msg of
                CMMaps ms -> do
                    logDbg $ "Calculation " ++ show cn ++ " handling CMMaps " ++ show (map xmapName ms)
                    atomically $ handleMaps rc ms
                    loop
                CMError e -> do
                    logDbg $ "Calculation " ++ show cn ++ " handling CMError " ++ show e
                    atomically $ handleError rc e
                    loop
                CMUpdateCalculation c -> do
                    logDbg $ "Calculation " ++ show cn ++ " handling CMUpdateCalculation " ++ show c
                    atomically $ handleCalculation rc c
                    loop
                CMViewStarted vc -> do
                    logDbg $ "Calculation " ++ show cn ++ " handling CMViewStarted"
                    atomically $ handleViewStarted rc vc
                    loop
                CMStop -> return ()
          logDbg t = atomically $ logDebug (logChan rc) "calc" t

runtimeCalcName :: RuntimeCalculation -> STM CalculationName
runtimeCalcName rc = do
    c <- readTVar (calculation rc)
    return (calculationName c)

handleCalculation :: RuntimeCalculation -> Calculation -> STM()
handleCalculation rc c = do
    writeTVar (calculation rc) c
    ers <- execAndSendIfFull rc
    case ers of
        Right () -> return ()
        Left err -> errorToAll rc err

handleError :: RuntimeCalculation -> Error -> STM ()
handleError = errorToAll

handleMaps :: RuntimeCalculation -> [XNamedMap] -> STM ()
handleMaps rc ms = do
    ers <- handleMapsWithErrors rc ms
    case ers of
        Right () -> return ()
        Left err -> errorToAll rc err

handleMapsWithErrors :: RuntimeCalculation -> [XNamedMap] -> STM (Either Error ())
handleMapsWithErrors rc ms = do
       modifyTVar (repository rc) (updateMapsInCalculation ms)
       execAndSendIfFull rc

updateMapsInCalculation :: [XNamedMap] -> MapRepository -> MapRepository
updateMapsInCalculation ms msbn = foldr  (\m msbni -> M.insert (xmapName m) (Just $ xmap m) msbni) msbn ms

handleViewStarted :: RuntimeCalculation -> ViewChan -> STM ()
handleViewStarted rc vc = do
    modifyTVar (viewsToNotify rc)  (L.union [vc])
    cr <- readTVar $ currentResult rc
    mapM_ (\m -> writeTChan vc (VMMaps [m])) cr

errorToAll :: RuntimeCalculation -> Error -> STM ()
errorToAll rc  e = do
    cs <- readTVar $ calculationsToNotify rc
    vs <- readTVar $ viewsToNotify rc
    sendToAll cs (CMError e)
    sendToAll vs (VMError e)
    logError (logChan rc) "calc" e
    return ()

execAndSendIfFull :: RuntimeCalculation ->  STM (Either Error ())
execAndSendIfFull rc = do
    cn <- runtimeCalcName rc
    logDebug (logChan rc) "calc" $ "ready to calculate formula " ++ show cn
    rm <- readTVar $ repository rc
    let mmbn = repositoryIfFull rm
    case mmbn of
       Just mbn -> execAndSendCalc mbn
       Nothing -> do
            logError (logChan rc) "calc" $ mkError ("no maps found to calculate formula " ++ show cn)
            return $ Right ()
    where execAndSendCalc mbn = do
            cn <- runtimeCalcName rc
            logDebug (logChan rc) "calc" $ "got maps to calculate formula" ++ show cn
            chs <- readTVar $ calculationsToNotify rc
            vs <- readTVar $ viewsToNotify rc
            execAndSend rc chs vs mbn

repositoryIfFull :: MapRepository -> Maybe XMapByName
repositoryIfFull rm = do
        lm <- mapM expandToKey (M.toList rm)
        return $ M.fromList lm
    where expandToKey (k, mv) = do v <- mv
                                   return (k, v)

execAndSend :: RuntimeCalculation -> [CalculationChan]  -> [ViewChan] -> XMapByName -> STM (Either Error ())
execAndSend rc cs vs mbn = do
    calc <- readTVar $ calculation rc
    logDebug (logChan rc) "calc" $ "calculating formula " ++ show (calculationName calc)
    let ers = execFormula (formula calc) mbn (operationMode calc)
    mapM (sendToDependents rc cs vs) ers

sendToDependents :: RuntimeCalculation -> [CalculationChan] -> [ViewChan] -> XMap -> STM ()
sendToDependents rc cs vs rs = do
    calc <- readTVar $ calculation rc
    let rsn = XNamedMap { xmapName = resultName calc, xmap = rs }
    writeTVar (currentResult rc) (Just rsn)
    sendToAll cs (CMMaps [rsn])
    sendToAll vs (VMMaps [rsn])
    return ()

