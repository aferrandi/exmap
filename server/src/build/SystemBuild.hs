module SystemBuild where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import SystemState
import Project
import ProjectJson
import Load
import CalculationActor
import ViewActor
import EventActor
import Dependencies
import AssocList
import Control.Concurrent

startSystem :: FilePath -> IO RuntimeSystem
startSystem root = do
    ps <- loadSystem root
    systemToRuntime root ps

systemToRuntime :: FilePath -> [ProjectName] -> IO RuntimeSystem
systemToRuntime root ps = do
    let psMap = mapWithNothingValues ps
    emptyByName <- newTVarIO psMap
    emptyByMapName <- newTVarIO M.empty
    eventChan <- newTChanIO
    forkIO $ actorEvent eventChan
    return RuntimeSystem {
        projectByName = emptyByName,
        projectByMapName = emptyByMapName,
        eventChan = eventChan,
        root = root
    }


loadSystem :: FilePath -> IO [ProjectName]
loadSystem root = do
    pe <- loadAvailableProjects root
    case pe of
        Right (AllProjects ps) -> return ps
        Left err -> return []
