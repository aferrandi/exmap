module EventMessages where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import WebMessages
import WebClients
import Errors

data EventMessage = EMWebEvent [WAClient] WebEvent
                    | EMStop

type EventChan = TChan EventMessage

mkErrorEvent :: String -> WebEvent
mkErrorEvent s = WEError $ mkError s

sendError :: EventChan -> [WAClient] -> Error -> STM ()
sendError chan cs err = writeTChan chan $ EMWebEvent cs (WEError err)

sendStringError :: EventChan -> [WAClient] -> String -> STM ()
sendStringError chan cs s = sendError chan cs (mkError s)