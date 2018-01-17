module WebAppState where


import Control.Concurrent.STM
import qualified Data.Map.Strict as M

import WebClients
import SystemMessages
import LogMessages
import Errors

type WAClientById = M.Map WAClientId WAClient

data WAState = WAState {
                    clients:: WAClientById,
                    systemChan :: SystemChan,
                    logChan :: LogChan
                    }

webLogDbg :: WAState -> String -> STM ()
webLogDbg state = logDebug (logChan state) "web"

webLogErr :: WAState -> Error -> STM ()
webLogErr state = logError(logChan state) "web"