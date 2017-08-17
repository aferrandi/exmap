module WebRequestsHandler where

import Store
import WebMessages
import WebClients
import SystemState

data WAState    =  WAState {
                    clients:: WAClients,
                    systemChan :: SystemChan
                    }

handleWebRequest :: WAClientId -> WAState -> WebRequest -> IO ()
handleWebRequest id s r = return ()


