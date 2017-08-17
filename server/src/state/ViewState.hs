module ViewState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)

import ActorMessages
import View
import XMapTypes
import WebClients


data RuntimeView =  RuntimeView {
    view :: View,
    subscribedClients :: [WAClient]
}

type ViewChan = TChan ViewMessage




