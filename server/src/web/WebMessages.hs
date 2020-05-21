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
    | WRAddMap ProjectName XNamedMap
    | WRUpdateMap ProjectName XNamedMap
    | WRSubscribeToView ProjectName ViewName
    | WRUnsubscribeFromView ProjectName ViewName
    | WRMapsInProject ProjectName
    | WRLoadView ProjectName ViewName
    | WRAddView ProjectName View
    | WRUpdateView ProjectName View
    | WRLoadCalculation ProjectName CalculationName
    | WRAddCalculation ProjectName CalculationSource
    | WRUpdateCalculation ProjectName CalculationSource
    | WRFunctions
    deriving (Show, Eq)

data WebEvent =
    WEAllProjects AllProjects
    | WEViewChanged ProjectName ViewName [XNamedMap]
    | WEProjectContent Project
    | WEProjectStored Project
    | WEMapLoaded ProjectName XNamedMap
    | WEMapAdded ProjectName XMapName Int
    | WEMapUpdated ProjectName XMapName Int
    | WEUnsubscribedFromView ProjectName ViewName
    | WEMapsInProject ProjectName [XMapDefinition]
    | WEViewStatus ProjectName View [XNamedMap]
    | WEViewLoaded ProjectName View
    | WEViewAdded ProjectName ViewName
    | WEViewUpdated ProjectName ViewName
    | WECalculationLoaded ProjectName CalculationSource
    | WECalculationAdded ProjectName CalculationName
    | WECalculationUpdated ProjectName CalculationName
    | WEFunctions Functions
    | WEInfo T.Text
    | WEError Error
    deriving (Show, Eq)

