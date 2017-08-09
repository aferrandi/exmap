module State where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)
import qualified Data.Text as T

import Formula
import XMapTypes
import View
import XFunction
import Project

data CalculationMessage = CMMap XNamedMap |
                          CMLog T.Text |
                          CMStop

data ProjectMessage = PMMap XNamedMap |
                      PMLog T.Text |
                      PMStop

data ViewMessage = VMMap XNamedMap |
                   VMLog T.Text |
                   VMStop

data LogMessage = LMLog T.Text |
                  LMStop


type LogChan = TChan LogMessage

type CalculationChan = TChan CalculationMessage

data RuntimeView =  RuntimeView {
    view :: View
}

type ViewChan = TChan ViewMessage

type MapRepository = M.Map XMapName (Maybe XMap)

data RuntimeCalculation = RuntimeCalculation {
    calculation :: Calculation,
    repository :: TVar MapRepository,
    calculationsToNotify :: [CalculationChan],
    viewsToNotify ::[ViewChan]
}

type CalculationChanByMap = M.Map XMapName [CalculationChan]
type ViewChanByMap = M.Map XMapName [ViewChan]

data RuntimeProject = RuntimeProject {
    project :: Project,
    calculationByMap :: TVar CalculationChanByMap,
    viewByMap :: TVar ViewChanByMap,
    logForViews :: LogChan
}

type ProjectChan = TChan ProjectMessage
type ProjectChanByName = M.Map ProjectName (Maybe ProjectChan)
type ProjectChansByMapName = M.Map XMapName [ProjectChan]

data RuntimeSystem = RuntimeSystem {
    projectByName :: TVar ProjectChanByName,
    projectByMapName :: TVar ProjectChansByMapName,
    logForProjects :: LogChan
}
