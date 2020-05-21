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


type WebEvent
    = WEAllProjects AllProjects
    | WEViewChanged ProjectName ViewName (List XNamedMap)
    | WEProjectContent Project
    | WEProjectStored Project
    | WEMapLoaded ProjectName XNamedMap
    | WEMapAdded ProjectName XMapName Int
    | WEMapUpdated ProjectName XMapName Int
    | WEUnsubscribedFromView ProjectName ViewName
    | WEMapsInProject ProjectName (List XMapDefinition)
    | WEViewStatus ProjectName View (List XNamedMap)
    | WEViewLoaded ProjectName View
    | WEViewAdded ProjectName ViewName
    | WEViewUpdated ProjectName ViewName
    | WECalculationLoaded ProjectName CalculationSource
    | WECalculationAdded ProjectName CalculationName
    | WECalculationUpdated ProjectName CalculationName
    | WEFunctions Functions
    | WEInfo String
    | WEError Error
