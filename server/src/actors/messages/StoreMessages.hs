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
                  | StMStoreMap ProjectChan WAClient ProjectName XNamedMap
                  | StMStoreCalculation ProjectChan WAClient ProjectName Calculation
                  | StMStoreView ProjectChan WAClient ProjectName View
                  | StMStop

type StoreChan = TChan StoreMessage
