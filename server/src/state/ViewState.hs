module ViewState where

import Control.Concurrent.STM.TVar (TVar)

import View
import WebClients
import XMapTypes
import Project
import LogMessages

data RuntimeView =  RuntimeView {
    runtimeViewName :: ViewName,
    ownerProjectName :: ProjectName,
    view :: TVar View,
    subscribedClients :: TVar [WAClient],
    mapsInView :: TVar XMapByName,
    logChan :: LogChan
}





