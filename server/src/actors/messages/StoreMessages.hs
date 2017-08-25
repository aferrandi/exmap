module StoreMessages where

import Control.Concurrent.STM.TChan (TChan)

import View
import Project
import ProjectMessages
import SystemMessages
import WebClients
import XMapTypes

data StoreMessage =
                  StMStoreProject SystemChan WAClient Project
                  | StMStoreMap ProjectChan WAClient ProjectName XNamedMap
                  | StMStop

type StoreChan = TChan StoreMessage
