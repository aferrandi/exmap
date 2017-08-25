module WebRequestsHandler where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Store
import WebMessages
import WebClients
import SystemState
import SystemMessages
import LogTypes
import LogMessages
import Errors

type WAClientById = M.Map WAClientId WAClient

data WAState = WAState {
                    clients:: WAClientById,
                    systemChan :: SystemChan,
                    logChan :: LogChan
                    }

handleWebRequest :: WAClientId -> WAState -> WebRequest -> STM ()
handleWebRequest id s r = do
    let mc = M.lookup id (clients s)
    case mc of
        Just c -> handleClientRequest c (systemChan s) r
        Nothing -> writeTChan (logChan s) (LogMLog $ mkError ("Client with id " ++ show id ++ " not found"))

handleClientRequest:: WAClient -> SystemChan -> WebRequest -> STM ()
handleClientRequest c sc r = case r of
                                WRLoadProject pn -> sendRequest $ SRLoadProject c pn
                                WRNewProject p -> sendRequest $ SRNewProject c p
                                WRUpdateProject p -> sendRequest $ SRUpdateProject c p
                                WRLoadMap pn mn -> sendRequest $ SRLoadMap c pn mn
                                WRStoreMap pn m -> sendRequest$ SRStoreMap c pn m
                                WRSubscribeToView pn vn -> sendRequest $ SRSubscribeToView c pn vn
                                WRUnsubscribeFromView pn vn -> sendRequest $ SRUnsubscribeFromView c pn vn
    where sendRequest r = writeTChan sc $ SMRequest r


