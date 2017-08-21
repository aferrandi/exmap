module ProjectBuild where

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
import EventMessages

projectToRuntime :: EventChan -> Project -> IO RuntimeProject
projectToRuntime evtChan p = do
        cs <- calculationChansByNames (calculations p)
        vs <- viewChansByNames evtChan (views p)
        csv <- newTVarIO cs
        vsv <- newTVarIO vs
        return RuntimeProject {
            project = p,
            calculationByMap = csv,
            viewByMap = vsv
       }

startProject :: EventChan -> FilePath -> ProjectName -> IO (Either String RuntimeProject)
startProject log root n = do
    pe <- loadProject root n
    mapM (projectToRuntime log) pe

