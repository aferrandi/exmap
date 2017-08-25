module LoadActor where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.Text.IO as TIO

import XMapTypes
import LoadMessages
import ProjectMessages
import SystemMessages
import Project
import View
import WebClients
import Load

actorLoad :: FilePath -> LoadChan -> IO ()
actorLoad root chan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                LMLoadView source c pn vn  -> do
                    loadViewInActor root source c pn vn
                    loop
                LMLoadProject source c pn -> do
                    loadProjectInActor root source c pn
                    loop
                LMLoadMap source c pn mn -> do
                    loadMapInActor root source c pn mn
                    loop
                LMStop -> return ()

loadViewInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> ViewName -> IO ()
loadViewInActor root source c pn vn = do
       mv <- loadView root pn vn
       case mv of
           Right v -> atomically $ writeTChan source (PMEvent $ PEViewLoaded c v)
           Left err -> atomically $ writeTChan source (PMEvent $ PEViewLoadError c vn err)

loadProjectInActor :: FilePath -> SystemChan -> WAClient -> ProjectName -> IO ()
loadProjectInActor root source c pn = do
       mp <- loadProject root pn
       case mp of
           Right p -> atomically $ writeTChan source (SMEvent $ SEProjectLoaded c p)
           Left err -> atomically $ writeTChan source (SMEvent $ SEProjectLoadError c pn err)

loadMapInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> XMapName -> IO ()
loadMapInActor root source c pn mn = do
       mm <- loadXMap root pn mn
       case mm of
           Right m -> atomically $ writeTChan source (PMEvent $ PEMapLoaded c m)
           Left err -> atomically $ writeTChan source (PMEvent $ PEMapLoadError c mn err)

