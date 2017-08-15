module CalculationBuild where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Traversable as T

import ProjectState
import CalculationState
import Project
import AssocList
import Dependencies
import CalculationActor



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


calculationToChan :: RuntimeCalculation -> STM CalculationChan
calculationToChan c = do
        ch <- newTChan
        actorCalculation ch c
        return ch

calculationChansByNames :: [Calculation] -> STM CalculationChanByMap
calculationChansByNames c = do
        rs <- mapM calculationToRuntime c
        let cs = M.fromList $ groupAssocListByKey (chanByDeps rs)
        T.mapM sequence cs
    where deps = calculationDependencies . calculation
          chansByDep cr = map (\dp -> (dp, calculationToChan cr)) (deps cr)
          chanByDeps rs =  concatMap chansByDep rs