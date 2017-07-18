module State where

import qualified Data.Map.Strict as M
import Formula
import XMapTypes
import View
import XFunction
import Project

newtype MapRepository = MapRepository (M.Map XMapName (Maybe XMap))

data RuntimeCalculation = RuntimeCalculation {
    calculation :: Calculation,
    repository :: MapRepository
}

newtype RuntimeCalculationByMap = RuntimeCalculationByMap ( M.Map XMapName RuntimeCalculation)

data RuntimeProject = RuntimeProject {
    project :: Project,
    calculationByMap :: RuntimeCalculationByMap
}

newtype RuntimeProjectByName = ProjectByName (M.Map ProjectName  (Maybe RuntimeProject))

data System = System {
    projectByName :: RuntimeProjectByName
}
