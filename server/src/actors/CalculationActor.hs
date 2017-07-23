module CalculationActor (actorCalculation) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import XMapTypes
import State
import Formula
import Project
import ExecFormula


actorCalculation :: TChan CalculationMessage -> RuntimeCalculation -> TChan LogMessage -> STM ()
actorCalculation chan rtCalc logChan = loop
    where loop = do
            msg <- readTChan chan
            case msg of
                (CMMap m) -> do
                    ers <- handleMap rtCalc m
                    case ers of
                        Right () -> return ()
                        Left err -> writeTChan logChan (LLog err)
                    loop
                CMStop -> return ()

handleMap :: RuntimeCalculation -> XNamedMap -> STM (Either T.Text ())
handleMap rtCalc m = do
       let trm = repository rtCalc
       modifyTVar trm (updateRepository m)
       rm <- readTVar trm
       let calc = calculation rtCalc
       let chs = dependentCalculations rtCalc
       case repositoryIfFull rm of
            Just xm -> execAndSend calc chs xm
            Nothing -> return $ Right ()
    where updateRepository m r = M.insert (xmapName m) (Just (xmap m)) r

repositoryIfFull :: MapRepository -> Maybe XMapByName
repositoryIfFull rm = do
        lm <- mapM expandToKey (M.toList rm)
        return $ M.fromList lm
    where expandToKey (k, mv) = do v <- mv
                                   return (k, v)

execAndSend :: Calculation -> [CalculationChan] -> XMapByName -> STM (Either T.Text ())
execAndSend calc chs xm = do
    let ers = execFormula (formula calc) xm  (operationMode calc)
    mapM (\rs -> sendToDependents calc chs rs) ers


sendToDependents :: Calculation -> [CalculationChan] -> XMap -> STM ()
sendToDependents calc chs xm = mapM_ sendToOne chs
    where sendToOne ch = writeTChan ch (CMMap resultWithName)
          resultWithName = XNamedMap { xmapName = resultName calc, xmap = xm }
