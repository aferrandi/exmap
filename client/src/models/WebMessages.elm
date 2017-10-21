module WebMessages exposing (..)

import XMapTypes exposing (..)
import Views exposing (..)
import Project exposing (..)
import Calculation exposing (..)

type WebRequest =
        WRAllProjects
        | WRSubscribeToProject ProjectName
        | WRNewProject Project
        | WRUpdateProject Project
        | WRLoadMaps ProjectName (List XMapName)
        | WRStoreMap ProjectName XNamedMap
        | WRSubscribeToView ProjectName ViewName
        | WRUnsubscribeFromView ProjectName ViewName
        | WRMapsInProject ProjectName
        | WRLoadView ProjectName ViewName
        | WRStoreView ProjectName View
        | WRLoadCalculation ProjectName CalculationName
        | WRStoreCalculation ProjectName CalculationSource
        | WRFunctions

type WebEvent =
        WEAllProjects AllProjects
        | WEViewChanged ProjectName ViewName (List XNamedMap)
        | WEProjectContent Project
        | WEProjectStored ProjectName
        | WEMapsLoaded ProjectName (List XNamedMap)
        | WEMapStored ProjectName XMapName
        | WEUnsubscribedFromView ProjectName ViewName
        | WEMapsInProject ProjectName (List XMapName)
        | WEViewStatus ProjectName View (List XNamedMap)
        | WEViewLoaded ProjectName View
        | WEViewStored ProjectName ViewName
        | WECalculationLoaded ProjectName CalculationSource
        | WECalculationStored ProjectName CalculationName
        | WEFunctions Functions
        | WEInfo String
        | WEError Error