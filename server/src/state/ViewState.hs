module ViewState where

import Control.Concurrent.STM.TVar (TVar)

import View
import WebClients

data RuntimeView =  RuntimeView {
    view :: TVar View,
    subscribedClients :: TVar [WAClient]
}





