{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module LoadActor (actorLoad) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad as CM
import qualified Data.Either as E
import Data.Bifunctor (first)

import XMapTypes
import LoadMessages
import ProjectMessages
import SystemMessages
import Project
import View
import Calculation
import WebClients
import Load
import Errors

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
                LMLoadMaps source c pn mns -> do
                    loadMapsInActor root source pn mns (PEMapsLoaded c) (PEMapsLoadError c)
                    loop
                LMLoadMapsForView source c pn vn mns -> do
                    loadMapsInActor root source pn mns (PEMapsForViewLoaded c vn) (PEMapsForViewLoadError c vn)
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
           Right p -> loadCalculationsInProject root source c p
           Left err -> atomically $ writeTChan source (SMEvent $ SEProjectLoadError c pn err)


loadCalculationsInProject :: FilePath -> SystemChan -> WAClient -> Project -> IO ()
loadCalculationsInProject root source c p = do
       cs <- mapM loadCalculationInProject (calculations p)
       atomically $ sendResults cs
       where sendResults :: [Either Error Calculation] -> STM ()
             sendResults cs = case E.lefts cs of
                        [] -> writeTChan source (SMEvent $ SEProjectLoaded c p (E.rights cs))
                        errs-> writeTChan source (SMEvent $ SEProjectLoadError c (projectName p) (compose errs))
             loadCalculationInProject = loadCalculation root (projectName p)


type MapsLoadedEvent = [XNamedMap] -> ProjectEvent
type MapsLoadErrorEvent = [XMapName] -> Error -> ProjectEvent


loadMapsInActor :: FilePath -> ProjectChan -> ProjectName -> [XMapName] -> MapsLoadedEvent -> MapsLoadErrorEvent -> IO ()
loadMapsInActor root source pn mns loadedEvent loadErrorEvent = do
       mmsOrErrs <- loadXMaps mns
       atomically $ sendResults mmsOrErrs
       where sendResults :: [Either Error XNamedMap] -> STM ()
             sendResults mmsOrErrs = do
                   let (errsWithName, mms) = E.partitionEithers (addMapNameToErrors mmsOrErrs mns)
                   CM.unless (null mms) $ writeTChan source (PMEvent $ loadedEvent mms)
                   CM.unless (null errsWithName) $ sendErrors errsWithName
             sendErrors :: [(Error, XMapName)] -> STM()
             sendErrors errsWithName = do
                let (errs, emns) = unzip errsWithName
                writeTChan source $ PMEvent (loadErrorEvent emns (compose errs))
             loadXMaps = mapM (loadXMap root pn)
             addMapNameToErrors = zipWith (\v mn -> first (\e -> (e, mn)) v)

