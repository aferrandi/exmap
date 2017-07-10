module State where

import qualified Data.Map.Strict as Map
import Formula
import XMapTypes
import View
import XFunction

newtype CalculationName = CalculationName String

newtype MapRepository = MapRepository (Map.Map XMapName (Maybe XMap))


data Calculation = Calculation {
    calculationName :: CalculationName,
    formula :: XFormula,
    maps :: MapRepository,
    operationMode :: OperationMode
}

newtype CalculationByMap = CalculationByMap ( Map.Map XMapName Calculation)

newtype ProjectName = ProjectName String

data Project = Project {
    projectName :: ProjectName,
    calculations :: [Calculation],
    views :: [View],
    calculationByMap :: CalculationByMap
}

newtype ProjectByName = ProjectByName ( Map.Map ProjectName Project)

data System = System {
    projectByName :: ProjectByName
}