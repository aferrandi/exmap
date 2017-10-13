module SystemMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Project
import WebClients
import View
import Calculation
import Errors()

data SystemRequest = SRSubscribeToProject WAClient ProjectName
                    | SRUnsubscribeFromProject WAClient ProjectName
                    | SRNewProject WAClient Project
                    | SRUpdateProject WAClient Project
                    | SRAllProjects WAClient
                    | SRLoadMaps WAClient ProjectName [XMapName]
                    | SRStoreMap WAClient ProjectName XNamedMap
                    | SRSubscribeToView WAClient ProjectName ViewName
                    | SRUnsubscribeFromView WAClient ProjectName ViewName
                    | SRLoadView WAClient ProjectName ViewName
                    | SRStoreView WAClient ProjectName View
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