module SystemState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)

import Project
import XMapTypes
import ProjectMessages
import CommonChannels

type ProjectChanByName = M.Map ProjectName (Maybe ProjectChan)
type ProjectChansByMapName = M.Map XMapName [ProjectChan]

data RuntimeSystem = RuntimeSystem {
    projectByName :: TVar ProjectChanByName,
    projectByMapName :: TVar ProjectChansByMapName,
    chans :: CommonChans,
    root :: FilePath
}
