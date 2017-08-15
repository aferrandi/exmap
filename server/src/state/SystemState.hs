module SystemState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)

import ProjectState
import ActorMessages


data RuntimeSystem = RuntimeSystem {
    projectByName :: TVar ProjectChanByName,
    projectByMapName :: TVar ProjectChansByMapName,
    logForProjects :: LogChan,
    root :: FilePath
}

type SystemChan = TChan SystemMessage