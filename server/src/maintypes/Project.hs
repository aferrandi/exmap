module Project where

-- strucure of a project that can be stored and read from a file

import qualified Data.Text as T
import qualified Data.List as L

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
    sourceOfMaps :: [XMapDefinition]
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

sourcesOfTypeInProject :: SourceType -> Project -> Maybe [XMapDefinition]
sourcesOfTypeInProject sc p = fmap sourceOfMaps fileSources
        where fileSources = L.find (\s -> sourceType s == sc) (sources p)
        
sourceOfMapsNames :: Source -> [XMapName]
sourceOfMapsNames s = L.map xmapName (sourceOfMaps s)

allMapsInProject:: Project -> [XMapDefinition]
allMapsInProject p = concatMap sourceOfMaps (sources p)
