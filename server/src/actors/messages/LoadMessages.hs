module LoadMessages where

import Control.Concurrent.STM.TChan (TChan)

import View
import Project
import ProjectMessages
import SystemMessages
import WebClients

data LoadMessage = LMLoadView ProjectChan WAClient ProjectName ViewName
                  | LMLoadProject SystemChan WAClient ProjectName
                  | LMStop

type LoadChan = TChan LoadMessage
