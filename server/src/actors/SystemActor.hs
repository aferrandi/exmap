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
import ProjectActor
import SystemMessages
import EventMessages
import LogMessages
import WebClients
import Project
import ProjectState
import Store

sendError :: WAClient -> String -> IO ()
sendError c s = WS.sendTextData (connection c) (T.pack s)

actorSystem :: SystemChan -> RuntimeSystem -> LogChan -> IO ()
actorSystem chan sys logChan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                SMLoadProject c pn -> do
                    loadProjectIfNotAlreadyRunning sys c pn
                    loop
                SMNewProject c p -> do
                    newProjectIfNotAlreadyRunning sys c p
                    loop
                SMStop -> return ()

newProjectIfNotAlreadyRunning :: RuntimeSystem -> WAClient -> Project -> IO ()
newProjectIfNotAlreadyRunning sys c p = do
    let pn = projectName p
    pbn <- atomically $ readTVar (projectByName sys)
    case M.lookup pn pbn of
        Just p -> sendError c $ "the project " ++ show pn ++ " exists already "
        Nothing -> newProject sys c p

newProject :: RuntimeSystem -> WAClient -> Project -> IO ()
newProject sys c p = do
    let pn = projectName p
    merr <- storeProject (root sys) p
    case merr of
       Nothing -> do
                    rp <- projectToRuntime (eventChan sys) p
                    runProject sys rp pn
       Just err -> sendError c $ "storing the project "  ++ " got " ++ (show err)


loadProjectIfNotAlreadyRunning :: RuntimeSystem -> WAClient -> ProjectName -> IO ()
loadProjectIfNotAlreadyRunning sys c pn= do
  pbn <- atomically $ readTVar (projectByName sys)
  case M.lookup pn pbn of
      Just mp -> case mp of
                     Just p -> sendError c $ "the project " ++ show pn ++ " is already running"
                     Nothing -> loadAndRunProject sys pn c
      Nothing -> sendError c $ "the project " ++ show pn ++ " does not exist in the sys"

loadAndRunProject :: RuntimeSystem -> ProjectName -> WAClient -> IO ()
loadAndRunProject sys pn c = do
  prjOrErr <- startProject (eventChan sys) (root sys) pn
  case prjOrErr of
    Right p -> runProject sys p pn
    Left err -> sendError c  $ "adding the project " ++ show pn ++ " got " ++ show err

runProject ::RuntimeSystem -> RuntimeProject -> ProjectName -> IO ()
runProject sys rp pn= do
    projectChan <- newTChanIO
    forkIO $ actorProject projectChan rp (eventChan sys)
    atomically $ modifyTVar (projectByName sys) (M.insert pn (Just projectChan))

