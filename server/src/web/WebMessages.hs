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
    | WRLoadMaps ProjectName [XMapName]
    | WRStoreMap ProjectName XNamedMap
    | WRSubscribeToView ProjectName ViewName
    | WRUnsubscribeFromView ProjectName ViewName
    | WRLoadView ProjectName ViewName
    | WRStoreView ProjectName View
    -- | WRCalculationStore ProjectName CalculationName CalculationFormulaText
    deriving (Show, Eq)

data WebEvent =
    WEAllProjects AllProjects
    | WEViewChanged ProjectName ViewName [XNamedMap]
    | WEProjectContent Project
    | WEProjectStored ProjectName
    | WEMapsLoaded ProjectName [XNamedMap]
    | WEMapStored ProjectName XMapName
    | WEUnsubscribedFromView ProjectName ViewName
    | WEViewStatus ProjectName View [XNamedMap]
    | WEViewLoaded ProjectName View
    | WEViewStored ProjectName ViewName
    | WEInfo T.Text
    | WEError Error
    deriving (Show, Eq)

