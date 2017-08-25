module ProjectMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import LogTypes
import Project
import WebClients
import View
import Errors

data ProjectRequest = PRMap XNamedMap
                        | PRUpdateProject Project
                        | PRSubscribeToView WAClient ViewName
                        | PRUnsubscribeFromView WAClient ViewName
                        | PRLoadMap WAClient XMapName
                        | PRStoreMap WAClient XNamedMap

data ProjectEvent = PEViewLoaded WAClient View
                    | PEViewLoadError WAClient ViewName Error
                    | PEMapLoaded WAClient XNamedMap
                    | PEMapLoadError WAClient XMapName Error
                    | PEMapStored WAClient XNamedMap
                    | PEMapStoreError WAClient XNamedMap Error

data ProjectMessage = PMRequest ProjectRequest
                      | PMEvent ProjectEvent
                      | PMStop

type ProjectChan = TChan ProjectMessage