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
        Nothing -> writeTChan (logChan s) (LMLog $ mkError ("Client with id " ++ show id ++ " not found"))

handleClientRequest:: WAClient -> SystemChan -> WebRequest -> STM ()
handleClientRequest c sc r = case r of
                                WRLoadProject pn -> writeTChan sc $ SMLoadProject c pn
                                WRNewProject p -> writeTChan sc $ SMNewProject c p
                                WRUpdateProject p -> writeTChan sc $ SMUpdateProject c p
                                WRLoadMap pn mn -> writeTChan sc $ SMLoadMap c pn mn
                                WRStoreMap pn m ->writeTChan sc $ SMStoreMap c pn m
                                WRSubscribeToView pn vn -> writeTChan sc $ SMSubscribeToView c pn vn
                                WRUnsubscribeFromView pn vn -> writeTChan sc $ SMUnsubscribeFromView c pn vn



