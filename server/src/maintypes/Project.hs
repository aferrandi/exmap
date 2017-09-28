module Project where

-- strucure of a project that can be stored and read from a file

import qualified Data.Text as T

import XMapTypes
import View
import Calculation


newtype ProjectName = ProjectName T.Text
    deriving (Show, Eq, Ord)

data SourceType = FileSource |
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
    views :: [ViewName],
    sources :: [Source] -- maps could come from different sources
} deriving (Show, Eq)


data User = User {
    userId :: T.Text,
    accessToProjects :: [ProjectName]
} deriving (Show, Eq)

newtype AllProjects = AllProjects [ProjectName]
    deriving (Show, Eq)