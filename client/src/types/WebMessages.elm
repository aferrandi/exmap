module WebMessages exposing (..)

import XMapTypes exposing (..)
import Views exposing (..)
import Project exposing (..)

type WebRequest =
        WRAllProjects
        | WRSubscribeToProject ProjectName
        | WRNewProject Project
        | WRUpdateProject Project
        | WRLoadMaps ProjectName (List XMapName)
        | WRStoreMap ProjectName XNamedMap
        | WRSubscribeToView ProjectName ViewName
        | WRUnsubscribeFromView ProjectName ViewName
        | WRLoadView ProjectName ViewName
        | WRStoreView ProjectName View
        | WRLoadCalculation ProjectName CalculationName
        | WRStoreCalculation ProjectName CalculationName CalculationFormulaText

type WebEvent =
        WEAllProjects AllProjects
        | WEViewChanged ProjectName ViewName (List XNamedMap)
        | WEProjectContent Project
        | WEProjectStored ProjectName
        | WEMapsLoaded ProjectName (List XNamedMap)
        | WEMapStored ProjectName XMapName
        | WEUnsubscribedFromView ProjectName ViewName
        | WEViewStatus ProjectName View (List XNamedMap)
        | WEViewLoaded ProjectName View
        | WEViewStored ProjectName ViewName
        | WECalculationLoaded ProjectName CalculationName CalculationFormulaText
        | WECalculationStored ProjectName CalculationName
        | WEInfo String
        | WEError Error