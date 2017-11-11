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
    rp <- newTVar (mapWithNothingValues deps)
    rc <- newTVar c
    cr <- newTVar Nothing
    vs <- newTVar []
    cs <- newTVar []
    return RuntimeCalculation {
        calculation = rc,
        repository = rp,
        currentResult = cr,
        calculationsToNotify = cs,
        viewsToNotify = vs,
        logChan = lc
    }
    where deps = formulaDependencies (formula c)


calculationToChan :: LogChan -> Calculation ->  IO CalculationChan
calculationToChan lc c = do
    cr <- atomically $ calculationToRuntime lc c
    ch <- newTChanIO
    _ <- forkIO $ actorCalculation ch cr
    return ch

