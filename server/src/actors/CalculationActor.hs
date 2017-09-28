{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CalculationActor (actorCalculation) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

import TChans
import XMapTypes
import CalculationState
import Calculation
import ExecFormula
import CalculationMessages
import ViewMessages


actorCalculation :: CalculationChan -> RuntimeCalculation -> IO ()
actorCalculation chan rtCalc = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                CMMap m -> do
                    atomically $ handleMap rtCalc m
                    loop
                CMError e -> do
                    atomically $ handleError rtCalc e
                    loop
                CMUpdateCalculation c -> do
                    atomically $ handleCalculation rtCalc c
                    loop
                CMStop -> return ()

handleCalculation :: RuntimeCalculation -> Calculation -> STM()
handleCalculation rtCalc c = do
    writeTVar (calculation rtCalc) c
    ers <- execAndSendIfFull rtCalc
    case ers of
        Right () -> return ()
        Left err -> errorToAll rtCalc err

handleError :: RuntimeCalculation -> Error -> STM ()
handleError = errorToAll

handleMap :: RuntimeCalculation -> XNamedMap -> STM ()
handleMap rtCalc m = do
    ers <- handleMapWithErrors rtCalc m
    case ers of
        Right () -> return ()
        Left err -> errorToAll rtCalc err

handleMapWithErrors :: RuntimeCalculation -> XNamedMap -> STM (Either Error ())
handleMapWithErrors rtCalc m = do
       modifyTVar (repository rtCalc) updateRepository
       execAndSendIfFull rtCalc
    where updateRepository = M.insert (xmapName m) (Just (xmap m))

errorToAll :: RuntimeCalculation -> Error -> STM ()
errorToAll rtCalc e = do
    let cs = calculationsToNotify rtCalc
    let vs = viewsToNotify rtCalc
    sendToAll cs (CMError e)
    sendToAll vs (VMError e)
    return ()

execAndSendIfFull :: RuntimeCalculation ->  STM (Either Error ())
execAndSendIfFull rtCalc = do
                            rm <- readTVar $ repository rtCalc
                            case repositoryIfFull rm of
                               Just xm -> execAndSendrtCalc xm
                               Nothing -> return $ Right ()
    where execAndSendrtCalc xm = do
                                 calc <- readTVar $ calculation rtCalc
                                 let chs = calculationsToNotify rtCalc
                                 let vs = viewsToNotify rtCalc
                                 execAndSend calc chs vs xm



repositoryIfFull :: MapRepository -> Maybe XMapByName
repositoryIfFull rm = do
        lm <- mapM expandToKey (M.toList rm)
        return $ M.fromList lm
    where expandToKey (k, mv) = do v <- mv
                                   return (k, v)

execAndSend :: Calculation -> [CalculationChan]  -> [ViewChan] -> XMapByName -> STM (Either Error ())
execAndSend calc cs vs xm = do
    let ers = execFormula (formula calc) xm  (operationMode calc)
    mapM (sendToDependents calc cs vs) ers

sendToDependents :: Calculation -> [CalculationChan] -> [ViewChan] -> XMap -> STM ()
sendToDependents calc cs vs rs = do
    let rsn = XNamedMap { xmapName = resultName calc, xmap = rs }
    sendToAll cs (CMMap rsn)
    sendToAll vs (VMMaps [rsn])
    return ()




