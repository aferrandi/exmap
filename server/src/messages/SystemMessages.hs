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
                    | SRLoadMap WAClient ProjectName XMapName
                    | SRAddMap WAClient ProjectName XNamedMap
                    | SRUpdateMap WAClient ProjectName XNamedMap
                    | SRSubscribeToView WAClient ProjectName ViewName
                    | SRUnsubscribeFromView WAClient ProjectName ViewName
                    | SRMapsInProject WAClient ProjectName
                    | SRLoadView WAClient ProjectName ViewName
                    | SRAddView WAClient ProjectName View
                    | SRUpdateView WAClient ProjectName View
                    | SRLoadCalculation WAClient ProjectName CalculationName
                    | SRAddCalculation WAClient ProjectName CalculationSource
                    | SRUpdateCalculation WAClient ProjectName CalculationSource
                    | SRFunctions WAClient
                    | SRDisconnect WAClient

    deriving (Show, Eq)

data SystemEvent = SEProjectLoaded WAClient Project [Calculation]
                    | SEProjectLoadError WAClient ProjectName Error
                    | SEProjectStored WAClient Project
                    | SEProjectStoreError WAClient Project Error
                    | SEAllProjectsStored WAClient AllProjects
                    | SEAllProjectsStoreError WAClient AllProjects Error
    deriving (Show, Eq)

data SystemMessage = SMRequest SystemRequest
                    | SMEvent SystemEvent
                    | SMStop
    deriving (Show, Eq)

type SystemChan = TChan SystemMessage