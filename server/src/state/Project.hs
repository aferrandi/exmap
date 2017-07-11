module Project where

-- strucure of a project that can be stored and read from a file

import qualified Data.Map.Strict as Map
import Formula
import XMapTypes
import View
import XFunction

newtype CalculationName = CalculationName String

data Calculation = Calculation {
    calculationName :: CalculationName,
    formula :: XFormula,
    maps :: [XMapName],
    operationMode :: OperationMode
}

newtype CalculationByMap = CalculationByMap ( Map.Map XMapName Calculation)

newtype ProjectName = ProjectName String


data SourceType = InternalSource |
              OdbcSource { connectionString :: String, sqlQuery :: String } |
              HttpSource { url :: String }

data Source = Source { sourceType :: SourceType, sourceOfMaps :: [XMapName] }

data Project = Project {
    projectName :: ProjectName,
    calculations :: [Calculation],
    views :: [View],
    sources :: [Source] -- maps could come from different sources
}

