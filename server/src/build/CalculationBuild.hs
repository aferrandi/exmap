module CalculationBuild where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent


import ProjectState
import CalculationState
import Project
import AssocList
import Dependencies
import CalculationActor
import CalculationMessages


calculationToRuntime :: Calculation -> STM RuntimeCalculation
calculationToRuntime c = do
    rp <- newTVar (mapWithNothingValues deps)
    return RuntimeCalculation {
        calculation = c,
        repository = rp,
        calculationsToNotify = [],
        viewsToNotify = []
    }
    where deps = formulaDependencies (formula c)


calculationToChan :: RuntimeCalculation -> IO CalculationChan
calculationToChan c = do
    ch <- newTChanIO
    forkIO $ actorCalculation ch c
    return ch

