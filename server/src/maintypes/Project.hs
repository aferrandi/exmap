module Project where

-- strucure of a project that can be stored and read from a file

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Formula
import XMapTypes
import View
import XFunction


newtype CalculationName = CalculationName T.Text
    deriving (Show, Eq, Ord)

data Calculation = Calculation {
    calculationName :: CalculationName,
    resultName :: XMapName,
    formula :: XFormula,
    operationMode :: OperationMode
} deriving (Show, Eq)

newtype ProjectName = ProjectName T.Text
    deriving (Show, Eq, Ord)

data SourceType = InternalSource |
      OdbcSource { connectionString :: T.Text, sqlQuery :: T.Text } |
      HttpSource { url :: T.Text }
    deriving (Show, Eq)

data Source = Source {
    sourceType :: SourceType,
    sourceOfMaps :: [XMapName]
} deriving (Show, Eq)

data Project = Project {
    projectName :: ProjectName,
    calculations :: [CalculationName],
    viewNames :: [ViewName],
    sources :: [Source] -- maps could come from different sources
} deriving (Show, Eq)


data User = User {
    userId :: T.Text,
    accessToProjects :: [ProjectName]
} deriving (Show, Eq)

newtype AllProjects = AllProjects [ProjectName]
