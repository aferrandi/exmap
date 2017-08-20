module EventMessages where

import Control.Concurrent.STM.TChan (TChan)

import WebMessages
import WebClients

data EventMessage = EMWebEvent [WAClient] WebEvent
                    | EMStop

type EventChan = TChan EventMessage