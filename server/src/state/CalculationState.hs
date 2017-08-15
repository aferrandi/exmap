module CalculationState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)

import Project
import XMapTypes
import ViewState
import ActorMessages

type MapRepository = M.Map XMapName (Maybe XMap)

type CalculationChan = TChan CalculationMessage

data RuntimeCalculation = RuntimeCalculation {
    calculation :: Calculation,
    repository :: TVar MapRepository,
    calculationsToNotify :: [CalculationChan],
    viewsToNotify ::[ViewChan]
}

type CalculationChanByMap = M.Map XMapName [CalculationChan]


