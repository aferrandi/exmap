module StoreMessages where

import Control.Concurrent.STM.TChan (TChan)

import View
import Project
import Calculation
import ProjectMessages
import SystemMessages
import WebClients
import XMapTypes

data StoreMessage =
                  StMStoreNewProject SystemChan WAClient Project
                  | StMStoreExistingProject ProjectChan WAClient Project
                  | StMStoreAllProjects SystemChan WAClient AllProjects
                  | StMStoreNewMap ProjectChan WAClient ProjectName XNamedMap
                  | StMStoreExistingMap ProjectChan WAClient ProjectName XNamedMap
                  | StMStoreNewCalculation ProjectChan WAClient ProjectName Calculation
                  | StMStoreExistingCalculation ProjectChan WAClient ProjectName Calculation
                  | StMStoreNewView ProjectChan WAClient ProjectName View
                  | StMStoreExistingView ProjectChan WAClient ProjectName View
                  | StMStop

type StoreChan = TChan StoreMessage
