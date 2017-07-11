module State where

import qualified Data.Map.Strict as Map
import Formula
import XMapTypes
import View
import XFunction
import Project

newtype MapRepository = MapRepository (Map.Map XMapName (Maybe XMap))

data RuntimeCalculation = RuntimeCalculation {
    calculation :: Calculation,
    repository :: MapRepository
}

data RuntimeProject = RuntimeProject {
    project :: Project,
    calculationByMap :: CalculationByMap
}

newtype RuntimeProjectByName = ProjectByName ( Map.Map ProjectName RuntimeProject)

data System = System {
    projectByName :: RuntimeProjectByName
}
