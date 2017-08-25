module StoreMessages where

import Control.Concurrent.STM.TChan (TChan)

import View
import Project
import ProjectMessages
import SystemMessages
import WebClients

data StoreMessage =
                  StMStoreProject SystemChan WAClient Project
                  | StMStop

type StoreChan = TChan StoreMessage
