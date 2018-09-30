module WebMessages where

import qualified Data.Text as T

import XMapTypes
import Project
import View
import Calculation

data WebRequest =
    WRAllProjects
    | WRSubscribeToProject ProjectName
    | WRNewProject Project
    | WRUpdateProject Project
    | WRLoadMap ProjectName XMapName
    | WRStoreMap ProjectName XNamedMap
    | WRSubscribeToView ProjectName ViewName
    | WRUnsubscribeFromView ProjectName ViewName
    | WRMapsInProject ProjectName
    | WRLoadView ProjectName ViewName
    | WRStoreView ProjectName View
    | WRLoadCalculation ProjectName CalculationName
    | WRStoreCalculation ProjectName CalculationSource
    | WRFunctions
    deriving (Show, Eq)

data WebEvent =
    WEAllProjects AllProjects
    | WEViewChanged ProjectName ViewName [XNamedMap]
    | WEProjectContent Project
    | WEProjectStored Project
    | WEMapLoaded ProjectName XNamedMap
    | WEMapStored ProjectName XMapName Int
    | WEUnsubscribedFromView ProjectName ViewName
    | WEMapsInProject ProjectName [XMapName]
    | WEViewStatus ProjectName View [XNamedMap]
    | WEViewLoaded ProjectName View
    | WEViewStored ProjectName ViewName
    | WECalculationLoaded ProjectName CalculationSource
    | WECalculationStored ProjectName CalculationName
    | WEFunctions Functions
    | WEInfo T.Text
    | WEError Error
    deriving (Show, Eq)

