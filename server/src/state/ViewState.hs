module ViewState where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)

import ActorMessages
import View
import XMapTypes


data RuntimeView =  RuntimeView {
    view :: View
}

type ViewChan = TChan ViewMessage

type ViewChanByMap = M.Map XMapName [ViewChan]



