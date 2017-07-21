module State where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)

import Formula
import XMapTypes
import View
import XFunction
import Project

data CalculationMessage = CMMap XNamedMap |
                          CMStop

data ProjectMessage = PMMap XNamedMap |
                      PMStop

type MapRepository = M.Map XMapName (Maybe XMap)

data RuntimeCalculation = RuntimeCalculation {
    calculation :: Calculation,
    repository :: TVar MapRepository
}

type CalculationChan = TChan CalculationMessage
type CalculationChansByMap = M.Map XMapName [CalculationChan]

data RuntimeProject = RuntimeProject {
    project :: Project,
    calculationByMap :: TVar CalculationChansByMap
}

type ProjectChan = TChan ProjectMessage
type ProjectChanByName = M.Map ProjectName (Maybe ProjectChan)
type ProjectChansByMapName = M.Map XMapName [ProjectChan]

data XSystem = XSystem {
    projectByName :: TVar ProjectChanByName,
    projectByMapName :: TVar ProjectChansByMapName
}
