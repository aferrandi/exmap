module WebRequestsHandler where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

import WebMessages
import WebClients
import SystemMessages
import LogMessages
import Errors
import WebAppState

handleWebRequest :: WAClientId -> WAState -> WebRequest -> STM ()
handleWebRequest cid s r = do
    webLogDbg s $ "handling request " ++ show r ++ " from " ++ show cid
    withClient cid s (\c -> handleClientRequest c (systemChan s) r)

withClient :: WAClientId -> WAState -> (WAClient -> STM()) -> STM ()
withClient cid s handle = do
    let mc = M.lookup cid (clients s)
    case mc of
        Just c -> handle c
        Nothing -> writeTChan (logChan s) (LogMErr "web" $ mkError ("Client with id " ++ show cid ++ " not found"))

handleClientRequest:: WAClient -> SystemChan -> WebRequest -> STM ()
handleClientRequest c sc r = case r of
                                WRAllProjects -> sendRequest  $ SRAllProjects c
                                WRSubscribeToProject pn -> sendRequest $ SRSubscribeToProject c pn
                                WRNewProject p -> sendRequest $ SRNewProject c p
                                WRUpdateProject p -> sendRequest $ SRUpdateProject c p
                                WRLoadMap pn mn -> sendRequest $ SRLoadMap c pn mn
                                WRAddMap pn m -> sendRequest$ SRAddMap c pn m
                                WRUpdateMap pn m -> sendRequest$ SRUpdateMap c pn m
                                WRSubscribeToView pn vn -> sendRequest $ SRSubscribeToView c pn vn
                                WRUnsubscribeFromView pn vn -> sendRequest $ SRUnsubscribeFromView c pn vn
                                WRMapsInProject pn -> sendRequest $ SRMapsInProject c pn
                                WRLoadView pn vn -> sendRequest $ SRLoadView c pn vn
                                WRAddView pn v -> sendRequest$ SRAddView c pn v
                                WRUpdateView pn v -> sendRequest$ SRUpdateView c pn v
                                WRLoadCalculation pn cn -> sendRequest$ SRLoadCalculation c pn cn
                                WRAddCalculation pn cs -> sendRequest$ SRAddCalculation c pn cs
                                WRUpdateCalculation pn cn -> sendRequest$ SRUpdateCalculation c pn cn
                                WRFunctions -> sendRequest  $ SRFunctions c
    where sendRequest sr = writeTChan sc $ SMRequest sr


