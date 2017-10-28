module SystemState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)

import Project
import XMapTypes
import ProjectMessages
import CommonChannels
import WebClients
import EventMessages (EventChan)

type ProjectChanByName = M.Map ProjectName (Maybe ProjectChan)
type ProjectChansByMapName = M.Map XMapName [ProjectChan]

data RuntimeSystem = RuntimeSystem {
    projectByName :: TVar ProjectChanByName,
    projectByMapName :: TVar ProjectChansByMapName,
    chans :: CommonChans,
    root :: FilePath,
    subscribedClients :: TVar [WAClient]
}

evtChan :: RuntimeSystem -> EventChan
evtChan sys = eventChan $ chans sys
