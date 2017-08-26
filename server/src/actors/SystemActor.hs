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
        SRLoadProject c pn -> loadProjectIfNotAlreadyRunning chan  sys c pn
        SRNewProject c p -> newProjectIfNotAlreadyRunning chan sys c p
        SRStoreMap c pn m -> pipeToProject c pn sys (PMRequest $ PRStoreMap c m)
        SRLoadMap c pn mn -> pipeToProject c pn sys (PMRequest $ PRLoadMap c mn)

handleEvent :: SystemChan -> RuntimeSystem -> SystemEvent -> IO ()
handleEvent chan sys e = case e of
        SEProjectLoaded c p -> projectLoaded sys c p
        SEProjectLoadError c pn err -> atomically $ sendSysError sys c ("loading the project "  ++ show pn ++ " got " ++ show err)
        SEProjectStored c p -> projectCreated sys c p
        SEProjectStoreError c p err -> atomically $ sendSysError sys c ("storing the project "  ++ show (projectName p) ++ " got " ++ show err)

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

projectCreated :: RuntimeSystem -> WAClient -> Project -> IO ()
projectCreated sys c p = do
            let pn = projectName p
            rp <- projectToRuntime (chans sys) p
            runProject sys rp pn

loadProjectIfNotAlreadyRunning :: SystemChan -> RuntimeSystem -> WAClient -> ProjectName -> STM ()
loadProjectIfNotAlreadyRunning chan sys c pn= do
  pbn <- readTVar (projectByName sys)
  case M.lookup pn pbn of
      Just mp -> case mp of
                     Just p -> sendSysError sys c ("the project " ++ show pn ++ " is already running")
                     Nothing -> writeTChan (loadChan $ chans sys) (LMLoadProject chan c pn)
      Nothing -> sendSysError sys c ("the project " ++ show pn ++ " does not exist in the sys")

projectLoaded :: RuntimeSystem -> WAClient -> Project -> IO ()
projectLoaded sys c p = do
            let pn = projectName p
            rp <- projectToRuntime (chans sys) p
            runProject sys rp pn


runProject ::RuntimeSystem -> PS.RuntimeProject -> ProjectName -> IO ()
runProject sys rp pn= do
    projectChan <- newTChanIO
    forkIO $ actorProject projectChan rp
    atomically $ modifyTVar (projectByName sys) (M.insert pn (Just projectChan))

sendSysError :: RuntimeSystem -> WAClient -> String -> STM ()
sendSysError sys c = sendStringError (eventChan $ chans sys) [c]