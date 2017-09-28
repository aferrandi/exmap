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


calculationToRuntime :: Calculation -> STM RuntimeCalculation
calculationToRuntime c = do
    rp <- newTVar (mapWithNothingValues deps)
    rc <- newTVar c
    return RuntimeCalculation {
        calculation = rc,
        repository = rp,
        calculationsToNotify = [],
        viewsToNotify = []
    }
    where deps = formulaDependencies (formula c)


calculationToChan :: Calculation ->  IO CalculationChan
calculationToChan c = do
    cr <- atomically $ calculationToRuntime c
    ch <- newTChanIO
    _ <- forkIO $ actorCalculation ch cr
    return ch

