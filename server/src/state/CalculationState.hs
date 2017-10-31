module CalculationState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)

import Calculation
import XMapTypes
import CalculationMessages
import ViewMessages
import LogMessages

type MapRepository = M.Map XMapName (Maybe XMap)

data RuntimeCalculation = RuntimeCalculation {
    calculation ::TVar Calculation,
    repository :: TVar MapRepository,
    currentResult :: TVar (Maybe XNamedMap),
    calculationsToNotify :: [CalculationChan],
    viewsToNotify ::[ViewChan],
    logChan :: LogChan
}


