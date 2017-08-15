module WebAppState where

import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

type WAClientId = Int

data WAClient   = WAClient {
    clientId :: WAClientId,
    connection :: WS.Connection
    }

type WAClients = [WAClient]

