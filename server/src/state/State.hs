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

data RuntimeProject = RuntimeProject {
    project :: Project,
    calculationByMap :: CalculationByMap
}

newtype RuntimeProjectByName = ProjectByName ( M.Map ProjectName RuntimeProject)

data System = System {
    projectByName :: RuntimeProjectByName
}
