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
                  | LMLoadMap ProjectChan WAClient ProjectName XMapName
                  | LMLoadCalculation ProjectChan WAClient ProjectName CalculationName
                  | LMStop

type LoadChan = TChan LoadMessage
