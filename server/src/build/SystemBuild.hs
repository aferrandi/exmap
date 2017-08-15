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
import LogActor
import Dependencies
import AssocList

startSystem :: FilePath -> IO RuntimeSystem
startSystem root = do
    ps <- loadSystem root
    atomically $ systemToRuntime root ps

systemToRuntime :: FilePath -> [ProjectName] -> STM RuntimeSystem
systemToRuntime root ps = do
    let psMap = mapWithNothingValues ps
    emptyByName <- newTVar psMap
    emptyByMapName <- newTVar M.empty
    log <- newTChan
    return RuntimeSystem {
        projectByName = emptyByName,
        projectByMapName = emptyByMapName,
        logForProjects = log,
        root = root
    }


loadSystem :: FilePath -> IO [ProjectName]
loadSystem root = do
    pe <- loadAvailableProjects root
    case pe of
        Right (AllProjects ps) -> return ps
        Left err -> return []
