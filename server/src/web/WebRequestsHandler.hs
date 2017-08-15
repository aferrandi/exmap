module WebRequestsHandler where

import Store
import WebMessages
import WebAppState
import SystemState

data WAState    =  WAState {
                    clients:: WAClients,
                    systemChan :: SystemChan
                    }

handleWebRequest :: WAClientId -> WAState -> WebRequest -> IO ()
handleWebRequest id s r = return ()


