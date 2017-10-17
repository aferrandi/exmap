module ProjectState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)

import ViewMessages
import CalculationMessages
import CommonChannels
import Project
import Calculation
import XMapTypes
import View
import WebClients

type ViewChanByName = M.Map ViewName ViewChan
type ViewChanByMap = M.Map XMapName [ViewChan]
type CalculationChanByMap = M.Map XMapName [CalculationChan]
type CalculationChanByName = M.Map CalculationName CalculationChan
type CalculationResultByName = M.Map CalculationName XMapName

data RuntimeProject = RuntimeProject {
    project :: TVar Project,
    calculationChanByName :: TVar CalculationChanByName,
    calculationChanByMap :: TVar CalculationChanByMap,
    calculationResultByName :: TVar CalculationResultByName,
    viewChanByMap :: TVar ViewChanByMap,
    viewChanByName :: TVar ViewChanByName,
    chans :: CommonChans,
    subscribedClients :: TVar [WAClient]
}



