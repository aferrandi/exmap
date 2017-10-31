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
                    print $ "handling LMLoadView " ++ show vn
                    loadViewInActor root source c pn vn
                    loop
                LMLoadProject source c pn -> do
                    print $ "handling LMLoadProject " ++ show pn
                    loadProjectInActor root source c pn
                    loop
                LMLoadMaps source c pn mns -> do
                    print $ "handling LMLoadMaps " ++ show mns
                    loadMapsInActor root source pn mns (PEMapsLoaded c) (PEMapsLoadError c)
                    loop
                LMLoadMapsForView source c pn vn mns -> do
                    print $ "handling LMLoadMapsForView " ++ show mns
                    loadMapsInActor root source pn mns (PEMapsForViewLoaded c vn) (PEMapsForViewLoadError c vn)
                    loop
                LMLoadMapsForCalculations source c pn mns -> do
                    print $ "handling LMLoadMapsForCalculations " ++ show mns
                    loadMapsInActor root source pn mns (PEMapsForCalculationsLoaded c) (PEMapsForCalculationsLoadError c)
                    loop
                LMLoadViewForProject source c pn vn  -> do
                    print $ "handling LMLoadViewForProject " ++ show vn
                    loadViewForProjectInActor root source c pn vn
                    loop
                LMLoadCalculation source c pn cn  -> do
                    print $ "handling LMLoadCalculation " ++ show cn
                    loadCalculationInActor root source c pn cn
                    loop
                LMStop -> return ()


loadViewInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> ViewName -> IO ()
loadViewInActor root source c pn vn = do
       mv <- loadView root pn vn
       case mv of
           Right v -> atomically $ writeTChan source (PMEvent $ PEViewLoaded c v)
           Left err -> atomically $ writeTChan source (PMEvent $ PEViewLoadError c vn err)

loadViewForProjectInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> ViewName -> IO ()
loadViewForProjectInActor root source c pn vn = do
       mv <- loadView root pn vn
       case mv of
           Right v -> atomically $ writeTChan source (PMEvent $ PEViewForProjectLoaded c v)
           Left err -> atomically $ writeTChan source (PMEvent $ PEViewForProjectLoadError c vn err)


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



loadCalculationInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> CalculationName -> IO ()
loadCalculationInActor root source c pn cn = do
       ce <- loadCalculation root pn cn
       atomically $ case ce of
           Right cc -> writeTChan source (PMEvent $ PECalculationLoaded c cc)
           Left err -> writeTChan source (PMEvent $ PECalculationLoadError c cn err)


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

