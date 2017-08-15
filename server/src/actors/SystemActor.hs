module SystemActor (actorSystem) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

import SystemState
import ProjectBuild
import ProjectActor
import ActorMessages
import WebAppState
import Project
import ProjectState


actorSystem :: SystemChan -> RuntimeSystem -> IO ()
actorSystem chan system = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                SMLoadProject c pn -> do
                    pbn <- atomically $ readTVar (projectByName system)
                    case M.lookup pn pbn of
                        Just p -> WS.sendTextData (connection c) (T.pack "already running")
                        Nothing -> buildProject system pn c
                    loop
                SMStop -> return ()

buildProject :: RuntimeSystem -> ProjectName -> WAClient -> IO ()
buildProject system pn c = do
  prjOrErr <- startProject (logForProjects system) (root system) pn
  case prjOrErr of
    Right p -> addProject system p pn
    Left err -> WS.sendTextData (connection c) (T.pack $ "adding the project " ++ show pn ++ " got " ++ show err)

addProject ::RuntimeSystem -> RuntimeProject -> ProjectName -> IO ()
addProject sys p pn= do
    projectChan <- newTChanIO
    actorProject projectChan p
    atomically $ modifyTVar (projectByName sys) (\pbn -> M.insert pn (Just projectChan) pbn)
