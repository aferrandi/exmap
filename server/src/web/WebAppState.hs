module WebAppState where

import State
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

type WAClientId = Int
type WAClient   = (WAClientId, WS.Connection)
type WAClients = [WAClient]
data WAState    =  WAState {
                    clients:: WAClients,
                    system :: RuntimeSystem
                    }