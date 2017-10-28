module SystemBuild (startSystem) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent

import SystemState
import Project
import Load
import EventActor
import LoadActor
import StoreActor
import AssocList
import LogMessages
import CommonChannels

startSystem :: FilePath -> LogChan -> IO RuntimeSystem
startSystem rt lch = do
    ps <- loadSystem rt
    systemToRuntime rt ps lch

systemToRuntime :: FilePath -> [ProjectName] -> LogChan -> IO RuntimeSystem
systemToRuntime rt ps lch  = do
    let psMap = mapWithNothingValues ps
    emptyByName <- newTVarIO psMap
    emptyByMapName <- newTVarIO M.empty
    chs <- buildChans rt lch
    tsc <- newTVarIO []
    return RuntimeSystem {
        projectByName = emptyByName,
        projectByMapName = emptyByMapName,
        chans = chs,
        root = rt,
        subscribedClients = tsc
    }

buildChans :: FilePath -> LogChan -> IO CommonChans
buildChans rt lch = do
    evch <- newTChanIO
    ldch <- newTChanIO
    stch <- newTChanIO
    _ <- forkIO $ actorEvent evch
    _ <- forkIO $ actorLoad rt ldch
    _ <- forkIO $ actorStore rt stch
    return CommonChans {
        eventChan = evch,
        loadChan = ldch,
        storeChan = stch,
        logChan = lch
    }


loadSystem :: FilePath -> IO [ProjectName]
loadSystem rt = do
    pe <- loadAvailableProjects rt
    case pe of
        Right (AllProjects ps) -> return ps
        Left _ -> return []
