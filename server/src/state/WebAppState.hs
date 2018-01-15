module WebAppState where

import WebClients
import SystemMessages
import LogMessages
import qualified Data.Map.Strict as M

type WAClientById = M.Map WAClientId WAClient

data WAState = WAState {
                    clients:: WAClientById,
                    systemChan :: SystemChan,
                    logChan :: LogChan
                    }