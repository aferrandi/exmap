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
    deriving (Show, Eq)

data SystemEvent = SEProjectLoaded WAClient Project [Calculation]
                    | SEProjectLoadError WAClient ProjectName Error
                    | SEProjectStored WAClient Project
                    | SEProjectStoreError WAClient Project Error
    deriving (Show, Eq)

data SystemMessage = SMRequest SystemRequest
                    | SMEvent SystemEvent
                    | SMStop
    deriving (Show, Eq)

type SystemChan = TChan SystemMessage