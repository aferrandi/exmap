{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CalculationActor (actorCalculation) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Debug.Trace

import TChans
import XMapTypes
import CalculationState
import Calculation
import ExecFormula
import CalculationMessages
import ViewMessages
import LogMessages


actorCalculation :: CalculationChan -> RuntimeCalculation -> IO ()
actorCalculation chan rc = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                CMMap m -> do
                    print $ "handling CMMap " ++ show (xmapName m)
                    atomically $ handleMap rc m
                    loop
                CMError e -> do
                    print $ "handling CMError " ++ show e
                    atomically $ handleError rc e
                    loop
                CMUpdateCalculation c -> do
                    print $ "handling CMUpdateCalculation " ++ show c
                    atomically $ handleCalculation rc c
                    loop
                CMViewStarted vc -> do
                    print "handling CMViewStarted"
                    atomically $ handleViewStarted rc vc
                    loop
                CMStop -> return ()

handleCalculation :: RuntimeCalculation -> Calculation -> STM()
handleCalculation rc c = do
    writeTVar (calculation rc) c
    ers <- execAndSendIfFull rc
    case ers of
        Right () -> return ()
        Left err -> errorToAll rc err

handleError :: RuntimeCalculation -> Error -> STM ()
handleError = errorToAll

handleMap :: RuntimeCalculation -> XNamedMap -> STM ()
handleMap rc m = do
    ers <- handleMapWithErrors rc m
    case ers of
        Right () -> return ()
        Left err -> errorToAll rc err

handleMapWithErrors :: RuntimeCalculation -> XNamedMap -> STM (Either Error ())
handleMapWithErrors rc m = do
       modifyTVar (repository rc) updateRepository
       execAndSendIfFull rc
    where updateRepository = M.insert (xmapName m) (Just (xmap m))

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
    writeTChan (logChan rc) (LogMLog e)
    return ()

execAndSendIfFull :: RuntimeCalculation ->  STM (Either Error ())
execAndSendIfFull rc = do
                            rm <- readTVar $ repository rc
                            let mmbn = repositoryIfFull rm
                            case trace ("calculation maps:" ++ show mmbn) mmbn of
                               Just mbn -> execAndSendCalc mbn
                               Nothing -> return $ Right ()
    where execAndSendCalc mbn = do
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
    calc <- trace ("execAndSend" ++ show (length vs)) (readTVar $ calculation rc)
    let ers = execFormula (formula calc) mbn (operationMode calc)
    mapM (sendToDependents rc cs vs) ers

sendToDependents :: RuntimeCalculation -> [CalculationChan] -> [ViewChan] -> XMap -> STM ()
sendToDependents rc cs vs rs = do
    calc <- trace "sendToDependents" (readTVar $ calculation rc)
    let rsn = XNamedMap { xmapName = resultName calc, xmap = rs }
    writeTVar (currentResult rc) (Just rsn)
    sendToAll cs (CMMap rsn)
    sendToAll vs (VMMaps [rsn])
    return ()




