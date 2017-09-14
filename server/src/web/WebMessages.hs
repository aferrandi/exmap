module WebMessages where

import qualified Data.Text as T

import XMapTypes
import Project
import View

data WebRequest =
    WRAllProjects
    | WRSubscribeToProject ProjectName
    | WRNewProject Project
    | WRUpdateProject Project
    | WRLoadMap ProjectName XMapName
    | WRStoreMap ProjectName XNamedMap
    | WRSubscribeToView ProjectName ViewName
    | WRUnsubscribeFromView ProjectName ViewName
    deriving (Show, Eq)

data WebEvent =
    WEAllProjects AllProjects
    | WEViewChanged ProjectName ViewName XNamedMap
    | WEProjectContent Project
    | WEProjectStored ProjectName
    | WEMapLoaded ProjectName XNamedMap
    | WEMapStored ProjectName XMapName
    | WEUnsubscribedFromView ProjectName ViewName
    | WEViewStatus ProjectName View [XNamedMap]
    | WEInfo T.Text
    | WEError Error
    deriving (Show, Eq)

