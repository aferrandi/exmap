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
                        | PRLoadMaps WAClient [XMapName]
                        | PRStoreMap WAClient XNamedMap
                        | PRStoreCalculation WAClient Calculation
                        | PRStoreView WAClient View
    deriving (Show, Eq)

data ProjectEvent = PEViewLoaded WAClient View
                    | PEViewLoadError WAClient ViewName Error
                    | PEMapsLoaded WAClient [XNamedMap]
                    | PEMapsLoadError WAClient [XMapName] Error
                    | PEMapsForViewLoaded WAClient ViewName [XNamedMap]
                    | PEMapsForViewLoadError WAClient ViewName [XMapName] Error
                    | PEMapStored WAClient XNamedMap
                    | PEMapStoreError WAClient XNamedMap Error
                    | PECalculationStored WAClient Calculation
                    | PECalculationStoreError WAClient Calculation Error
                    | PEViewStored WAClient View
                    | PEViewStoreError WAClient View Error
                    | PEProjectStored WAClient Project
                    | PEProjectStoreError WAClient Project Error

    deriving (Show, Eq)

data ProjectMessage = PMRequest ProjectRequest
                      | PMEvent ProjectEvent
                      | PMStop
    deriving (Show, Eq)

type ProjectChan = TChan ProjectMessage