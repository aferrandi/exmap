module CalculationState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)

import Project
import XMapTypes
import CalculationMessages
import ViewMessages

type MapRepository = M.Map XMapName (Maybe XMap)

data RuntimeCalculation = RuntimeCalculation {
    calculation :: Calculation,
    repository :: TVar MapRepository,
    calculationsToNotify :: [CalculationChan],
    viewsToNotify ::[ViewChan]
}


