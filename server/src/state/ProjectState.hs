module ProjectState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM (STM, readTVar)


import ViewMessages
import CalculationMessages
import CommonChannels
import Project
import Calculation
import XMapTypes
import View
import WebClients
import EventMessages (EventChan)

data CalculationResult = CalculationResult {
    calcName :: CalculationName,
    resultDef :: XMapDefinition
} deriving (Show, Eq)

type ViewChanByName = M.Map ViewName ViewChan
type ViewChanByMap = M.Map XMapName [ViewChan]
type CalculationChanByMap = M.Map XMapName [CalculationChan]
type CalculationChanByName = M.Map CalculationName CalculationChan
type CalculationResultByResultName = M.Map XMapName CalculationResult

data RuntimeProject = RuntimeProject {
    project :: TVar Project,
    calculationChanByName :: TVar CalculationChanByName,
    calculationChanByMap :: TVar CalculationChanByMap,
    calculationByResult :: TVar CalculationResultByResultName,
    viewChanByMap :: TVar ViewChanByMap,
    viewChanByName :: TVar ViewChanByName,
    chans :: CommonChans,
    subscribedClients :: TVar [WAClient]
}

prjName :: RuntimeProject -> STM ProjectName
prjName rp = do
    p <- readTVar (project rp)
    return $ projectName p

evtChan :: RuntimeProject -> EventChan
evtChan rp = eventChan $ chans rp


