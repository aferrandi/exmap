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

-- because not all views of the project are necessarily been viewed
type ViewChanByName = M.Map ViewName (Maybe ViewChan)
type ViewChanByMapName = M.Map XMapName [ViewChan]
type CalculationChanByMapName = M.Map XMapName [CalculationChan]

data RuntimeProject = RuntimeProject {
    project :: Project,
    calculationChanByMapName :: TVar CalculationChanByMapName,
    viewChanByMap :: TVar ViewChanByMapName,
    viewChanByName :: TVar ViewChanByName,
    chans :: CommonChans
}



