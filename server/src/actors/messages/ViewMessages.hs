module ViewMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Errors
import WebClients

data ViewMessage = VMMap XNamedMap
                      | VMSubscribeToView WAClient
                      | VMUnsubscribeFromView WAClient
                      | VMError Error
                      | VMStop
    deriving (Show, Eq)

type ViewChan = TChan ViewMessage
