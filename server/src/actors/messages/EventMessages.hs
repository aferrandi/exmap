module EventMessages where

import qualified Data.Text as T
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import WebMessages
import WebClients
import Errors

data EventMessage = EMWebEvent [WAClient] WebEvent
                    | EMStop
    deriving (Show, Eq)

type EventChan = TChan EventMessage

mkErrorEvent :: String -> WebEvent
mkErrorEvent s = WEError $ mkError s

sendError :: EventChan -> [WAClient] -> Error -> STM ()
sendError chan cs err = writeTChan chan $ EMWebEvent cs (WEError err)

sendStringError :: EventChan -> [WAClient] -> String -> STM ()
sendStringError chan cs s = sendError chan cs (mkError s)

sendInfo :: EventChan -> [WAClient] -> String -> STM ()
sendInfo chan cs s = writeTChan chan $ EMWebEvent cs (WEInfo (T.pack s))
