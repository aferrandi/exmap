module ViewMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Errors()
import WebClients
import View

data ViewMessage = VMMaps [XNamedMap]
                      | VMSubscribeToView WAClient
                      | VMUnsubscribeFromView WAClient
                      | VMUpdate View
                      | VMError Error
                      | VMStop
    deriving (Show, Eq)

data ViewChan = ViewChan {
    vcChannel :: TChan ViewMessage,
    vcName :: ViewName
} deriving (Eq)
