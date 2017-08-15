module CalculationActor (actorCalculation) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import XMapTypes
import CalculationState
import ViewState
import Formula
import Project
import ExecFormula
import ActorMessages


actorCalculation :: CalculationChan -> RuntimeCalculation -> STM ()
actorCalculation chan rtCalc = loop
    where loop = do
            msg <- readTChan chan
            case msg of
                CMMap m -> do
                    ers <- handleMap rtCalc m
                    case ers of
                        Right () -> return ()
                        Left err -> logToAll rtCalc err
                    loop
                CMLog t -> logToAll rtCalc t
                CMStop -> return ()

logToAll :: RuntimeCalculation -> T.Text -> STM ()
logToAll rtCalc  t = do
    let cs = calculationsToNotify rtCalc
    let vs = viewsToNotify rtCalc
    sendToAll cs (CMLog t)
    sendToAll vs (VMLog t)
    return ()

handleMap :: RuntimeCalculation -> XNamedMap -> STM (Either T.Text ())
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

execAndSend :: Calculation -> [CalculationChan]  -> [ViewChan] -> XMapByName -> STM (Either T.Text ())
execAndSend calc cs vs xm = do
    let ers = execFormula (formula calc) xm  (operationMode calc)
    mapM (sendToDependents calc cs vs) ers

sendToDependents :: Calculation -> [CalculationChan]  -> [ViewChan] -> XMap -> STM ()
sendToDependents calc cs vs rs = do
    let rsn = XNamedMap { xmapName = resultName calc, xmap = rs }
    sendToAll cs (CMMap rsn)
    sendToAll vs (VMMap rsn)
    return ()


sendToAll :: [TChan a] -> a -> STM ()
sendToAll chs xm = mapM_ sendToOne chs
    where sendToOne ch = writeTChan ch xm

