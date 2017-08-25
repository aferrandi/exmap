module SystemMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Project
import WebClients
import View
import LogTypes
import Errors

data SystemRequest = SRLoadProject WAClient ProjectName
                    | SRNewProject WAClient Project
                    | SRUpdateProject WAClient Project
                    | SRLoadMap WAClient ProjectName XMapName
                    | SRStoreMap WAClient ProjectName XNamedMap
                    | SRSubscribeToView WAClient ProjectName ViewName
                    | SRUnsubscribeFromView WAClient ProjectName ViewName


data SystemEvent = SEProjectLoaded WAClient Project
                    | SEProjectLoadError WAClient ProjectName Error
                    | SEProjectStored WAClient Project
                    | SEProjectStoreError WAClient Project Error


data SystemMessage = SMRequest SystemRequest
                    | SMEvent SystemEvent
                    | SMStop

type SystemChan = TChan SystemMessage