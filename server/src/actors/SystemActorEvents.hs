{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module SystemActorEvents where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent

import SystemState
import ProjectBuild
import SystemMessages
import ProjectMessages
import EventMessages
import StoreMessages
import CommonChannels
import ProjectActor (actorProject)
import WebClients
import WebMessages
import Project
import Calculation
import qualified ProjectState as PS

handleEvent :: SystemChan -> RuntimeSystem -> SystemEvent -> IO ()
handleEvent chan sys e = case e of
        SEProjectLoaded c p cs -> projectLoaded sys c p cs
        SEProjectLoadError c pn err -> atomically $ sendSysError sys c ("loading the project "  ++ show pn ++ " got " ++ show err)
        SEProjectStored c p -> atomically $ projectStored chan sys c p
        SEProjectStoreError c p err -> atomically $ sendSysError sys c ("storing the project "  ++ show (projectName p) ++ " got " ++ show err)
        SEAllProjectsStored c _ -> atomically $ allProjectStored sys c
        SEAllProjectsStoreError c ap err -> atomically $ sendSysError sys c ("storing all projects "  ++ show ap ++ " got " ++ show err)


projectLoaded :: RuntimeSystem -> WAClient -> Project -> [Calculation] -> IO ()
projectLoaded sys c p cs = do
    let pn = projectName p
    rp <- projectToRuntime (chans sys) p cs
    runProject sys rp pn
    atomically $ pipeToProject c pn sys (PMRequest (PRSubscribeToProject c))

projectStored :: SystemChan -> RuntimeSystem -> WAClient -> Project -> STM ()
projectStored chan sys c p = do
    let pn = projectName p
    modifyTVar (projectByName sys) (M.insertWith (\_ old -> old) pn Nothing)
    pbn <- readTVar $ projectByName sys
    cs <- readTVar $ subscribedClients sys
    let ap = AllProjects (M.keys pbn)
    writeTChan (evtChan sys) (EMWebEvent cs $ WEAllProjects ap)
    sendInfo (evtChan sys) [c] ("Project " ++ show pn ++ " stored")
    writeTChan chan $ SMRequest( SRSubscribeToProject c pn)
    writeTChan (storeChan $ chans sys) $ StMStoreAllProjects chan c ap

runProject ::RuntimeSystem -> PS.RuntimeProject -> ProjectName -> IO ()
runProject sys rp pn= do
    projectChan <- newTChanIO
    _ <- forkIO $ actorProject projectChan rp
    atomically $ modifyTVar (projectByName sys) (M.insert pn (Just projectChan))

allProjectStored :: RuntimeSystem -> WAClient -> STM ()
allProjectStored sys c = do
    sendInfo (evtChan sys) [c] (" All projects stored")

sendSysError :: RuntimeSystem -> WAClient -> String -> STM ()
sendSysError sys c = sendStringError (evtChan sys) [c]

pipeToProject :: WAClient -> ProjectName  -> RuntimeSystem-> ProjectMessage -> STM()
pipeToProject c pn sys msg = do
    pbn <- readTVar $ projectByName sys
    case M.lookup pn pbn of
        Just mprjChan -> case mprjChan of
            Just prjChan -> writeTChan prjChan msg
            Nothing -> sendSysError sys c ("the project " ++ show pn ++ " was not loaded")
        Nothing -> sendSysError sys c ("the project " ++ show pn ++ " does not exist")