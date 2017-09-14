module SystemActor (actorSystem) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Exit (die)

import SystemState
import ProjectBuild
import SystemMessages
import ProjectMessages
import EventMessages
import WebMessages
import ProjectActor (actorProject)
import LogMessages
import LoadMessages
import CommonChannels
import WebClients
import Project
import qualified ProjectState as PS
import Store

actorSystem :: SystemChan -> RuntimeSystem -> IO ()
actorSystem chan sys = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                SMRequest r -> do
                    atomically $ handleRequest chan sys r
                    loop
                SMEvent e -> do
                    handleEvent chan sys e
                    loop
                SMStop -> return ()
                otherwise -> die $ "Unexpected message " ++ show msg ++ " in system actor"


handleRequest :: SystemChan -> RuntimeSystem -> SystemRequest -> STM ()
handleRequest chan sys r = case r of
        SRAllProjects c -> allProjects c sys
        SRSubscribeToProject c pn -> loadProjectIfNotAlreadyRunning chan  sys c pn

        SRNewProject c p -> newProjectIfNotAlreadyRunning chan sys c p
        SRStoreMap c pn m -> pipeToProject c pn sys (PMRequest $ PRStoreMap c m)
        SRLoadMap c pn mn -> pipeToProject c pn sys (PMRequest $ PRLoadMap c mn)

handleEvent :: SystemChan -> RuntimeSystem -> SystemEvent -> IO ()
handleEvent chan sys e = case e of
        SEProjectLoaded c p cs -> projectLoaded sys c p cs
        SEProjectLoadError c pn err -> atomically $ sendSysError sys c ("loading the project "  ++ show pn ++ " got " ++ show err)
        SEProjectStored c p -> atomically $ sendInfo (eventChan $ chans sys) [c] ("Project " ++ show (projectName p) ++ " loaded")
        SEProjectStoreError c p err -> atomically $ sendSysError sys c ("storing the project "  ++ show (projectName p) ++ " got " ++ show err)

allProjects :: WAClient -> RuntimeSystem -> STM ()
allProjects c sys = do
    pbn <- readTVar $ projectByName sys
    let ps = AllProjects $ M.keys pbn
    let evtChan  = eventChan $ chans sys
    writeTChan evtChan (EMWebEvent [c] $ WEAllProjects ps)

pipeToProject :: WAClient -> ProjectName  -> RuntimeSystem-> ProjectMessage -> STM()
pipeToProject c pn sys msg = do
    pbn <- readTVar $ projectByName sys
    case M.lookup pn pbn of
        Just mprjChan -> case mprjChan of
            Just prjChan -> writeTChan prjChan msg
            Nothing -> sendSysError sys c ("the project " ++ show pn ++ " was not loaded")
        Nothing -> sendSysError sys c ("the project " ++ show pn ++ " does not exist")


newProjectIfNotAlreadyRunning :: SystemChan -> RuntimeSystem -> WAClient -> Project -> STM ()
newProjectIfNotAlreadyRunning chan sys c p = do
    let pn = projectName p
    pbn <- readTVar $ projectByName sys
    case M.lookup pn pbn of
        Just prjChan -> sendSysError sys c ("the project " ++ show pn ++ " exists already ")
        Nothing -> writeTChan (loadChan $ chans sys) (LMLoadProject chan c pn)

loadProjectIfNotAlreadyRunning :: SystemChan -> RuntimeSystem -> WAClient -> ProjectName -> STM ()
loadProjectIfNotAlreadyRunning chan sys c pn= do
  pbn <- readTVar (projectByName sys)
  case M.lookup pn pbn of
      Just mpc -> case mpc of
                     Just pc -> writeTChan pc (PMRequest (PRSubscribeToProject c))
                     Nothing -> writeTChan (loadChan $ chans sys) (LMLoadProject chan c pn)
      Nothing -> sendSysError sys c ("the project " ++ show pn ++ " does not exist in the sys")


projectLoaded :: RuntimeSystem -> WAClient -> Project -> [Calculation] -> IO ()
projectLoaded sys c p cs = do
    let pn = projectName p
    rp <- projectToRuntime (chans sys) p cs
    runProject sys rp pn
    atomically $ pipeToProject c pn sys (PMRequest (PRSubscribeToProject c))


runProject ::RuntimeSystem -> PS.RuntimeProject -> ProjectName -> IO ()
runProject sys rp pn= do
    projectChan <- newTChanIO
    forkIO $ actorProject projectChan rp
    atomically $ modifyTVar (projectByName sys) (M.insert pn (Just projectChan))

sendSysError :: RuntimeSystem -> WAClient -> String -> STM ()
sendSysError sys c = sendStringError (eventChan $ chans sys) [c]