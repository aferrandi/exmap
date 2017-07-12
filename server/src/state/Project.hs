module Project where

-- strucure of a project that can be stored and read from a file

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Formula
import XMapTypes
import View
import XFunction

newtype CalculationName = CalculationName T.Text

data Calculation = Calculation {
    calculationName :: CalculationName,
    formula :: XFormula,
    maps :: [XMapName],
    operationMode :: OperationMode
}

newtype CalculationByMap = CalculationByMap ( Map.Map XMapName Calculation)

newtype ProjectName = ProjectName T.Text


data SourceType = InternalSource |
              OdbcSource { connectionString :: T.Text, sqlQuery :: T.Text } |
              HttpSource { url :: T.Text }

data Source = Source { sourceType :: SourceType, sourceOfMaps :: [XMapName] }

data Project = Project {
    projectName :: ProjectName,
    calculations :: [Calculation],
    views :: [View],
    sources :: [Source] -- maps could come from different sources
}


data User = User {
    userId :: T.Text,
    accessToProjects :: [ProjectName]
}
