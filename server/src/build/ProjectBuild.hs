module ProjectBuild (startProject) where

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import CalculationState
import ProjectState
import ViewState
import Project
import ProjectJson
import Load
import CalculationActor
import ViewActor
import LogActor
import Dependencies
import CalculationBuild
import ViewBuild

projectToRuntime :: LogChan -> Project -> STM RuntimeProject
projectToRuntime log p = do
        cs <- calculationChansByNames (calculations p)
        vs <- viewChansByNames log (views p)
        csv <- newTVar cs
        vsv <- newTVar vs
        return RuntimeProject {
            project = p,
            calculationByMap = csv,
            viewByMap = vsv,
            logForViews = log
       }

startProject :: LogChan -> FilePath -> ProjectName -> IO (Either String RuntimeProject)
startProject log root n = do
    pe <- loadProject root n
    atomically $ mapM (projectToRuntime log) pe

