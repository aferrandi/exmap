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
import LogMessages
import Project
import View
import Calculation
import WebClients
import Load
import Errors

actorLoad :: FilePath -> LoadChan -> LogChan -> IO ()
actorLoad root chan lch = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                LMLoadViewForClient source c pn vn  -> do
                    logDbg $ "handling LMLoadView " ++ show vn
                    loadViewForClient root source c pn vn
                    loop
                LMLoadProject source c pn -> do
                    logDbg $ "handling LMLoadProject " ++ show pn
                    loadProjectInActor root source c pn
                    loop
                LMLoadMapForClient source c pn mn -> do
                    logDbg $ "handling LMLoadMapForClient " ++ show mn
                    loadMapForClient root source c pn mn
                    loop
                LMLoadMapsForView source c pn vn mns -> do
                    logDbg $ "handling LMLoadMapsForView " ++ show mns
                    loadMapsInActor root source pn mns (PEMapsForViewLoaded c vn) (PEMapsForViewLoadError c vn)
                    loop
                LMLoadMapsForCalculations source c pn mns -> do
                    logDbg $ "handling LMLoadMapsForCalculations " ++ show mns
                    loadMapsInActor root source pn mns (PEMapsForCalculationsLoaded c) (PEMapsForCalculationsLoadError c)
                    loop
                LMLoadMapsForCalculation source c pn cn mns -> do
                    logDbg $ "handling LMLoadMapsForCalculation " ++ show mns
                    loadMapsInActor root source pn mns (PEMapsForCalculationLoaded c cn) (PEMapsForCalculationLoadError c cn)
                    loop
                LMLoadViewForProject source c pn vn  -> do
                    logDbg $ "handling LMLoadViewForProject " ++ show vn
                    loadViewForProjectInActor root source c pn vn
                    loop
                LMLoadCalculation source c pn cn  -> do
                    logDbg $ "handling LMLoadCalculation " ++ show cn
                    loadCalculationInActor root source c pn cn
                    loop
                LMStop -> return ()
          logDbg t = atomically $ logDebug lch "load" t

loadViewForClient :: FilePath -> ProjectChan -> WAClient -> ProjectName -> ViewName -> IO ()
loadViewForClient root source c pn vn = do
       mv <- loadView root pn vn
       case mv of
           Right v -> atomically $ writeTChan source (PMEvent $ PEViewForClientLoaded c v)
           Left err -> atomically $ writeTChan source (PMEvent $ PEViewForClientLoadError c vn err)

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
           Right cc -> writeTChan source (PMEvent $ PECalculationForClientLoaded c cc)
           Left err -> writeTChan source (PMEvent $ PECalculationForClientLoadError c cn err)


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


loadMapForClient :: FilePath -> ProjectChan -> WAClient -> ProjectName -> XMapName -> IO ()
loadMapForClient root source c pn mn = do
       mOrErr <- loadXMap root pn mn
       atomically $ case mOrErr of
            Left err -> writeTChan source (PMEvent $ PEMapForClientLoadError c mn err)
            Right m ->  writeTChan source (PMEvent $ PEMapForClientLoaded c m)
