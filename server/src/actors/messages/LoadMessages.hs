module LoadMessages where

import Control.Concurrent.STM.TChan (TChan)

import View
import Project
import ProjectMessages
import SystemMessages
import WebClients
import XMapTypes
import Calculation

data LoadMessage = LMLoadViewForClient ProjectChan WAClient ProjectName ViewName
                  | LMLoadProject SystemChan WAClient ProjectName
                  | LMLoadMapForClient ProjectChan WAClient ProjectName XMapName
                  | LMLoadMapsForView ProjectChan WAClient ProjectName ViewName [XMapName]
                  | LMLoadMapsForCalculations ProjectChan WAClient ProjectName [XMapName]
                  | LMLoadViewForProject ProjectChan WAClient ProjectName ViewName
                  | LMLoadCalculation ProjectChan WAClient ProjectName CalculationName
                  | LMStop

type LoadChan = TChan LoadMessage
