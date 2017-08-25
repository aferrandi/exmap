module SystemBuild where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent

import SystemState
import Project
import ProjectJson
import Load
import CalculationActor
import LoadMessages
import ViewActor
import EventActor
import LoadActor
import StoreActor
import Dependencies
import AssocList
import LogMessages
import CommonChannels

startSystem :: FilePath -> LogChan -> IO RuntimeSystem
startSystem root logChan = do
    ps <- loadSystem root
    systemToRuntime root ps logChan

systemToRuntime :: FilePath -> [ProjectName] -> LogChan -> IO RuntimeSystem
systemToRuntime root ps logChan  = do
    let psMap = mapWithNothingValues ps
    emptyByName <- newTVarIO psMap
    emptyByMapName <- newTVarIO M.empty
    chans <- buildChans
    return RuntimeSystem {
        projectByName = emptyByName,
        projectByMapName = emptyByMapName,
        chans = chans,
        root = root
    }
    where buildChans :: IO CommonChans
          buildChans = do
            eventChan <- newTChanIO
            loadChan <- newTChanIO
            storeChan <- newTChanIO
            forkIO $ actorEvent eventChan
            forkIO $ actorLoad root loadChan
            forkIO $ actorStore root storeChan
            return CommonChans {
                eventChan = eventChan,
                loadChan = loadChan,
                storeChan = storeChan,
                logChan = logChan
            }


loadSystem :: FilePath -> IO [ProjectName]
loadSystem root = do
    pe <- loadAvailableProjects root
    case pe of
        Right (AllProjects ps) -> return ps
        Left err -> return []
