module ProjectBuild where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import CalculationState
import ProjectState
import ViewState
import CommonChannels
import Project
import ProjectJson
import Load
import CalculationActor
import Dependencies
import CalculationBuild
import ViewBuild
import Errors

projectToRuntime :: CommonChans -> Project -> IO RuntimeProject
projectToRuntime chans p = do
        cs <- calculationChansByNames (calculations p)
        cbm <- newTVarIO cs
        rp <- newTVarIO p
        vbm <- newTVarIO M.empty
        vbn <- newTVarIO M.empty
        return RuntimeProject {
            project = rp,
            calculationChanByMapName = cbm,
            viewChanByMap = vbm,
            viewChanByName = vbn,
            chans = chans
       }


