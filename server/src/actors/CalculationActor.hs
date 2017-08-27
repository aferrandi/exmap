module CalculationActor (actorCalculation) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Exit (die)

import TChans
import XMapTypes
import CalculationState
import ViewState
import Formula
import Project
import ExecFormula
import CalculationMessages
import ViewMessages
import LogTypes


actorCalculation :: CalculationChan -> RuntimeCalculation -> IO ()
actorCalculation chan rtCalc = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                CMMap m -> do
                    ers <- atomically $ handleMap rtCalc m
                    case ers of
                        Right () -> return ()
                        Left err -> atomically $ errorToAll rtCalc err
                    loop
                CMError e -> atomically $ errorToAll rtCalc e
                CMStop -> return ()
                otherwise -> die $ "Unexpected message " ++ show msg ++ " in calculation actor"

errorToAll :: RuntimeCalculation -> Error -> STM ()
errorToAll rtCalc e = do
    let cs = calculationsToNotify rtCalc
    let vs = viewsToNotify rtCalc
    sendToAll cs (CMError e)
    sendToAll vs (VMError e)
    return ()

handleMap :: RuntimeCalculation -> XNamedMap -> STM (Either Error ())
handleMap rtCalc m = do
       let trm = repository rtCalc
       modifyTVar trm (updateRepository m)
       rm <- readTVar trm
       let calc = calculation rtCalc
       let chs = calculationsToNotify rtCalc
       let vs = viewsToNotify rtCalc
       case repositoryIfFull rm of
            Just xm -> execAndSend calc chs vs xm
            Nothing -> return $ Right ()
    where updateRepository m r = M.insert (xmapName m) (Just (xmap m)) r

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

sendToDependents :: Calculation -> [CalculationChan]  -> [ViewChan] -> XMap -> STM ()
sendToDependents calc cs vs rs = do
    let rsn = XNamedMap { xmapName = resultName calc, xmap = rs }
    sendToAll cs (CMMap rsn)
    sendToAll vs (VMMap rsn)
    return ()




