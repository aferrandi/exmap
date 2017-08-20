module SystemState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)

import Project
import XMapTypes
import ProjectState
import ProjectMessages
import EventMessages

type ProjectChanByName = M.Map ProjectName (Maybe ProjectChan)
type ProjectChansByMapName = M.Map XMapName [ProjectChan]

data RuntimeSystem = RuntimeSystem {
    projectByName :: TVar ProjectChanByName,
    projectByMapName :: TVar ProjectChansByMapName,
    eventChan :: EventChan,
    root :: FilePath
}
