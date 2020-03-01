module ProjectMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Project
import WebClients
import View
import Calculation

data ProjectRequest =     PRSubscribeToProject WAClient
                        | PRUnsubscribeFromProject WAClient
                        | PRUpdateProject WAClient Project
                        | PRSubscribeToView WAClient ViewName
                        | PRUnsubscribeFromView WAClient ViewName
                        | PRMapsInProject WAClient
                        | PRLoadMapForClient WAClient XMapName
                        | PRAddMap WAClient XNamedMap
                        | PRUpdateMap WAClient XNamedMap
                        | PRLoadCalculationForClient WAClient CalculationName
                        | PRAddCalculation WAClient CalculationSource
                        | PRUpdateCalculation WAClient CalculationSource
                        | PRLoadViewForClient WAClient ViewName
                        | PRAddView WAClient View
                        | PRUpdateView WAClient View
                        | PRStartCalculations WAClient
                        | PRDisconnect WAClient


    deriving (Show, Eq)

data ProjectEvent = PEViewForClientLoaded WAClient View
                    | PEViewForClientLoadError WAClient ViewName Error
                    | PEMapForClientLoaded WAClient XNamedMap
                    | PEMapForClientLoadError WAClient XMapName Error
                    | PEMapsForViewLoaded WAClient ViewName [XNamedMap]
                    | PEMapsForViewLoadError WAClient ViewName [XMapName] Error
                    | PEMapsForCalculationsLoaded WAClient [XNamedMap]
                    | PEMapsForCalculationsLoadError WAClient [XMapName] Error
                    | PEMapsForCalculationLoaded WAClient CalculationName [XNamedMap]
                    | PEMapsForCalculationLoadError WAClient CalculationName [XMapName] Error
                    | PEMapAdded WAClient XNamedMap
                    | PEMapAddError WAClient XNamedMap Error
                    | PEMapUpdated WAClient XNamedMap
                    | PEMapUpdateError WAClient XNamedMap Error
                    | PECalculationAdded WAClient Calculation
                    | PECalculationAddError WAClient Calculation Error
                    | PECalculationUpdated WAClient Calculation
                    | PECalculationUpdateError WAClient Calculation Error
                    | PECalculationForClientLoadError WAClient CalculationName Error
                    | PECalculationForClientLoaded WAClient Calculation
                    | PEViewAdded WAClient View
                    | PEViewAddError WAClient View Error
                    | PEViewUpdated WAClient View
                    | PEViewUpdateError WAClient View Error
                    | PEProjectStored WAClient Project
                    | PEProjectStoreError WAClient Project Error
                    | PEViewForProjectLoaded WAClient View
                    | PEViewForProjectLoadError WAClient ViewName Error

    deriving (Show, Eq)

data ProjectMessage = PMRequest ProjectRequest
                      | PMEvent ProjectEvent
                      | PMStop
    deriving (Show, Eq)

type ProjectChan = TChan ProjectMessage