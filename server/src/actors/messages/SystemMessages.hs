module SystemMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Project
import WebClients
import View
import LogTypes

data SystemMessage = SMLoadProject WAClient ProjectName
                    | SMNewProject WAClient Project
                    | SMUpdateProject WAClient Project
                    | SMLoadMap WAClient ProjectName XMapName
                    | SMStoreMap WAClient ProjectName XNamedMap
                    | SMSubscribeToView WAClient ProjectName ViewName
                    | SMUnsubscribeFromView WAClient ProjectName ViewName
                    | SMStop

type SystemChan = TChan SystemMessage