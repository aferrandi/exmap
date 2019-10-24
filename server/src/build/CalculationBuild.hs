module CalculationBuild where

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Set as S

import CalculationState
import Calculation
import XMapTypes
import AssocList
import Dependencies
import CalculationActor
import CalculationMessages
import LogMessages



calculationToRuntime :: LogChan -> S.Set XMapName -> Calculation -> STM RuntimeCalculation
calculationToRuntime lc rs c = do
    mr <- newTVar (mapWithNothingValues inputMapsNotProducedByCalculations)
    rc <- newTVar c
    cr <- newTVar Nothing
    vs <- newTVar []
    cs <- newTVar []
    logDebug lc "calc" $ "Maps used by calculation " ++ show (calculationName c) ++ ": " ++ (show inputMapsNotProducedByCalculations)
    return RuntimeCalculation {
        calculation = rc,
        mapRepository = mr,
        currentResult = cr,
        calculationsToNotify = cs,
        viewsToNotify = vs,
        logChan = lc
    }
    where inputMapsNotProducedByCalculations = filter (\mn -> S.notMember mn rs) $ calculationDependenciesMaps c


calculationToChan :: LogChan -> S.Set XMapName -> Calculation ->  IO CalculationChan
calculationToChan lc rs c = do
    cr <- atomically $ calculationToRuntime lc rs c
    ch <- newTChanIO
    let cch = CalculationChan {
          ccChannel = ch,
          ccName = calculationName c
    }
    _ <- forkIO $ actorCalculation cch cr
    return cch
