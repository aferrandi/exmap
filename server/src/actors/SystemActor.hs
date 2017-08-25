module SystemActor (actorSystem) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import SystemState
import ProjectBuild
import SystemMessages
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
                    handleRequest chan sys r
                    loop
                SMEvent e -> do
                    handleEvent chan sys e
                    loop
                SMStop -> return ()


handleRequest :: SystemChan -> RuntimeSystem -> SystemRequest -> IO ()
handleRequest chan sys r = case r of
        SRLoadProject c pn ->
            loadProjectIfNotAlreadyRunning chan  sys c pn
        SRNewProject c p ->
            newProjectIfNotAlreadyRunning chan sys c p

handleEvent :: SystemChan -> RuntimeSystem -> SystemEvent -> IO ()
handleEvent chan sys e = case e of
        SEProjectLoaded c p -> projectLoaded sys c p
        SEProjectLoadError c pn err -> atomically $ sendSysError sys c ("loading the project "  ++ show pn ++ " got " ++ show err)
        SEProjectStored c p -> projectCreated sys c p
        SEProjectStoreError c p err -> atomically $ sendSysError sys c ("storing the project "  ++ show (projectName p) ++ " got " ++ show err)

newProjectIfNotAlreadyRunning :: SystemChan -> RuntimeSystem -> WAClient -> Project -> IO ()
newProjectIfNotAlreadyRunning chan sys c p = do
    let pn = projectName p
    pbn <- atomically $ readTVar (projectByName sys)
    case M.lookup pn pbn of
        Just p -> atomically $ sendSysError sys c ("the project " ++ show pn ++ " exists already ")
        Nothing -> atomically $ writeTChan (loadChan $ chans sys) (LMLoadProject chan c pn)

projectCreated :: RuntimeSystem -> WAClient -> Project -> IO ()
projectCreated sys c p = do
            let pn = projectName p
            rp <- projectToRuntime (chans sys) p
            runProject sys rp pn

loadProjectIfNotAlreadyRunning :: SystemChan -> RuntimeSystem -> WAClient -> ProjectName -> IO ()
loadProjectIfNotAlreadyRunning chan sys c pn= do
  pbn <- atomically $ readTVar (projectByName sys)
  case M.lookup pn pbn of
      Just mp -> case mp of
                     Just p -> atomically $ sendSysError sys c ("the project " ++ show pn ++ " is already running")
                     Nothing -> atomically $ writeTChan (loadChan $ chans sys) (LMLoadProject chan c pn)
      Nothing -> atomically $ sendSysError sys c ("the project " ++ show pn ++ " does not exist in the sys")

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