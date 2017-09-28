module LoadMessages where

import Control.Concurrent.STM.TChan (TChan)

import View
import Project
import ProjectMessages
import SystemMessages
import WebClients
import XMapTypes

data LoadMessage = LMLoadView ProjectChan WAClient ProjectName ViewName
                  | LMLoadProject SystemChan WAClient ProjectName
                  | LMLoadMaps ProjectChan WAClient ProjectName [XMapName]
                  | LMLoadMapsForView ProjectChan WAClient ProjectName ViewName [XMapName]
                  | LMStop

type LoadChan = TChan LoadMessage
