module ProjectMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import LogTypes
import Project
import WebClients
import View
import Errors

data ProjectRequest = PRMap XNamedMap
                        | PRSubscribeToProject WAClient
                        | PRUnsubscribeFromProject WAClient
                        | PRUpdateProject Project
                        | PRSubscribeToView WAClient ViewName
                        | PRUnsubscribeFromView WAClient ViewName
                        | PRLoadMap WAClient XMapName
                        | PRStoreMap WAClient XNamedMap
    deriving (Show, Eq)

data ProjectEvent = PEViewLoaded WAClient View
                    | PEViewLoadError WAClient ViewName Error
                    | PEMapLoaded WAClient XNamedMap
                    | PEMapLoadError WAClient XMapName Error
                    | PEMapStored WAClient XNamedMap
                    | PEMapStoreError WAClient XNamedMap Error
                    | PECalculationStored WAClient Calculation
                    | PECalculationStoreError WAClient Calculation Error
                    | PEViewStored WAClient View
                    | PEViewStoreError WAClient View Error
    deriving (Show, Eq)

data ProjectMessage = PMRequest ProjectRequest
                      | PMEvent ProjectEvent
                      | PMStop
    deriving (Show, Eq)

type ProjectChan = TChan ProjectMessage