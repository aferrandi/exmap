module ProjectState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)

import ViewMessages
import CalculationMessages
import EventMessages
import Project
import XMapTypes

type ViewChanByMap = M.Map XMapName [ViewChan]
type CalculationChanByMap = M.Map XMapName [CalculationChan]

data RuntimeProject = RuntimeProject {
    project :: Project,
    calculationByMap :: TVar CalculationChanByMap,
    viewByMap :: TVar ViewChanByMap
}



