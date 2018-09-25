module SystemActorRequests where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Maybe as B
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import TextEnums
import SystemState
import SystemMessages
import ProjectMessages
import EventMessages
import WebMessages
import LoadMessages
import StoreMessages
import CommonChannels
import WebClients
import Project
import Calculation
import OperationTypes

handleRequest :: SystemChan -> RuntimeSystem -> SystemRequest -> STM ()
handleRequest chan sys r = case r of
        SRAllProjects c -> allProjects c sys
        SRSubscribeToProject c pn -> loadProjectIfNotAlreadyRunning chan  sys c pn
        SRUnsubscribeFromProject c pn ->pipeToProject c pn sys (PMRequest $ PRUnsubscribeFromProject c)
        SRSubscribeToView c pn vn -> pipeToProject c pn sys (PMRequest $ PRSubscribeToView c vn)
        SRUnsubscribeFromView c pn vn -> pipeToProject c pn sys (PMRequest $ PRUnsubscribeFromView c vn)
        SRMapsInProject c pn -> pipeToProject c pn sys (PMRequest $ PRMapsInProject c)
        SRNewProject c p -> newProjectIfNotAlreadyRunning chan sys c p
        SRUpdateProject c p -> pipeToProject c (projectName p) sys (PMRequest $ PRUpdateProject c p)
        SRStoreMap c pn m -> pipeToProject c pn sys (PMRequest $ PRStoreMap c m)
        SRLoadMap c pn mn -> pipeToProject c pn sys (PMRequest $ PRLoadMapForClient c mn)
        SRLoadView c pn vn -> pipeToProject c pn sys (PMRequest $ PRLoadViewForClient c vn)
        SRStoreView c pn v -> pipeToProject c pn sys (PMRequest $ PRStoreView c v)
        SRLoadCalculation c pn cn -> pipeToProject c pn sys (PMRequest $ PRLoadCalculationForClient c cn)
        SRStoreCalculation c pn cs -> pipeToProject c pn sys (PMRequest $ PRStoreCalculation c cs)
        SRFunctions c -> sendFunctions sys c
        SRDisconnect c -> disconnectClient sys c

allProjects :: WAClient -> RuntimeSystem -> STM ()
allProjects c sys = do
    addSubscriber sys c
    pbn <- readTVar $ projectByName sys
    let ps = AllProjects $ M.keys pbn
    writeTChan (evtChan sys) (EMWebEvent [c] $ WEAllProjects ps)

pipeToProject :: WAClient -> ProjectName  -> RuntimeSystem-> ProjectMessage -> STM()
pipeToProject c pn sys msg = do
    pbn <- readTVar $ projectByName sys
    case M.lookup pn pbn of
        Just mprjChan -> case mprjChan of
            Just prjChan -> writeTChan prjChan msg
            Nothing -> sendSysError sys c ("the project " ++ show pn ++ " was not loaded")
        Nothing -> sendSysError sys c ("the project " ++ show pn ++ " does not exist")

sendFunctions :: RuntimeSystem -> WAClient -> STM()
sendFunctions sys c = do
    let fs = Functions { operationTypes = allOperationTypes }
    writeTChan (evtChan sys) (EMWebEvent [c] $ WEFunctions fs)

newProjectIfNotAlreadyRunning :: SystemChan -> RuntimeSystem -> WAClient -> Project -> STM ()
newProjectIfNotAlreadyRunning chan sys c p = do
    let pn = projectName p
    pbn <- readTVar $ projectByName sys
    case M.lookup pn pbn of
        Just _ -> sendSysError sys c ("the project " ++ show pn ++ " exists already ")
        Nothing -> writeTChan (storeChan $ chans sys) $ StMStoreNewProject chan c p

loadProjectIfNotAlreadyRunning :: SystemChan -> RuntimeSystem -> WAClient -> ProjectName -> STM ()
loadProjectIfNotAlreadyRunning chan sys c pn= do
  pbn <- readTVar (projectByName sys)
  case M.lookup pn pbn of
      Just mpc -> case mpc of
                     Just pc -> writeTChan pc (PMRequest $ PRSubscribeToProject c)
                     Nothing -> writeTChan (loadChan $ chans sys) (LMLoadProject chan c pn)
      Nothing -> sendSysError sys c ("the project " ++ show pn ++ " does not exist in the sys")

disconnectClient :: RuntimeSystem -> WAClient -> STM()
disconnectClient sys c = do
    removeSubscriber sys c
    disconnectClientFromAllProjects
    where disconnectClientFromAllProjects =  do
           pbn <- readTVar $ projectByName sys
           mapM_ (\pc -> writeTChan pc (PMRequest $ PRDisconnect c)) (B.catMaybes $ M.elems pbn)


sendSysError :: RuntimeSystem -> WAClient -> String -> STM ()
sendSysError sys c = sendStringError (evtChan sys) [c]

addSubscriber ::RuntimeSystem -> WAClient -> STM ()
addSubscriber sys c= modifyTVar (subscribedClients sys) (L.union [c])

removeSubscriber :: RuntimeSystem -> WAClient -> STM()
removeSubscriber sys c = modifyTVar (subscribedClients sys) (filter notSameClient)
    where notSameClient ci = ci /= c

