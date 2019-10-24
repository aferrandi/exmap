module CalculationBuild where

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent

import CalculationState
import Calculation
import AssocList
import Dependencies
import CalculationActor
import CalculationMessages
import LogMessages


calculationToRuntime :: LogChan -> Calculation -> STM RuntimeCalculation
calculationToRuntime lc c = do
    mr <- newTVar (mapWithNothingValues deps)
    rc <- newTVar c
    cr <- newTVar Nothing
    vs <- newTVar []
    cs <- newTVar []
    logDebug lc "calc" $ "Dependencies of calculation " ++ show (calculationName c) ++ ": " ++ (show deps)
    return RuntimeCalculation {
        calculation = rc,
        mapRepository = mr,
        currentResult = cr,
        calculationsToNotify = cs,
        viewsToNotify = vs,
        logChan = lc
    }
    where deps = calculationDependenciesMaps c


calculationToChan :: LogChan -> Calculation ->  IO CalculationChan
calculationToChan lc c = do
    cr <- atomically $ calculationToRuntime lc c
    ch <- newTChanIO
    let cch = CalculationChan {
          ccChannel = ch,
          ccName = calculationName c
    }
    _ <- forkIO $ actorCalculation cch cr
    return cch
