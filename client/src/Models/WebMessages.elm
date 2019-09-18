module Models.WebMessages exposing (..)

import Types.Calculation exposing (..)
import Types.Project exposing (..)
import Types.Views exposing (..)
import Types.XMapTypes exposing (..)


type WebRequest
    = WRAllProjects
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


type WebEvent
    = WEAllProjects AllProjects
    | WEViewChanged ProjectName ViewName (List XNamedMap)
    | WEProjectContent Project
    | WEProjectStored Project
    | WEMapLoaded ProjectName XNamedMap
    | WEMapStored ProjectName XMapName Int
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
