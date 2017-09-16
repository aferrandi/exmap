module ProjectState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)

import ViewMessages
import CalculationMessages
import CommonChannels
import Project
import XMapTypes
import View
import WebClients

-- because not all views of the project are necessarily been viewed
type ViewChanByName = M.Map ViewName (Maybe ViewChan)
type ViewChanByMap = M.Map XMapName [ViewChan]
type CalculationChanByMap = M.Map XMapName [CalculationChan]
type CalculationChanByName = M.Map CalculationName CalculationChan

data RuntimeProject = RuntimeProject {
    project :: TVar Project,
    calculationChanByName :: TVar CalculationChanByName,
    calculationChanByMap :: TVar CalculationChanByMap,
    viewChanByMap :: TVar ViewChanByMap,
    viewChanByName :: TVar ViewChanByName,
    chans :: CommonChans,
    subscribedClients :: TVar [WAClient]
}



