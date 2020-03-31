{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module StoreActor (actorStore) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import XMapTypes

import StoreMessages
import ProjectMessages
import SystemMessages
import LogMessages
import Project
import Calculation
import View
import WebClients
import Store

actorStore :: FilePath -> StoreChan -> LogChan -> IO ()
actorStore root chan lch = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                StMStoreNewProject source c p -> do
                    logDbg $ "handling StMStoreNewProject " ++ show p
                    storeNewProjectInActor root source c p
                    loop
                StMStoreExistingProject source c p -> do
                    logDbg $ "handling StMStoreExistingProject " ++ show p
                    storeExistingProjectInActor root source c p
                    loop
                StMStoreAllProjects source c ap -> do
                    logDbg $ "handling StMStoreAllProjects " ++ show ap
                    storeAllProjectsInActor root source c ap
                    loop
                StMStoreNewMap source c pn m -> do
                    logDbg $ "handling StMStoreMap " ++ show (mapName m) ++ " for project " ++ show pn
                    storeNewMapInActor root source c pn m
                    loop
                StMStoreExistingMap source c pn m -> do
                    logDbg $ "handling StMStoreMap " ++ show (mapName m) ++ " for project " ++ show pn
                    storeExistingMapInActor root source c pn m
                    loop
                StMStoreNewCalculation source c pn clc -> do
                    logDbg $ "handling StMStoreCalculation " ++ show (calculationName clc) ++ " for project " ++ show pn
                    storeNewCalculationInActor root source c pn clc
                    loop
                StMStoreExistingCalculation source c pn clc -> do
                    logDbg $ "handling StMStoreCalculation " ++ show (calculationName clc) ++ " for project " ++ show pn
                    storeExistingCalculationInActor root source c pn clc
                    loop
                StMStoreNewView source c pn v -> do
                    logDbg $ "handling StMStoreView " ++ show (viewName v) ++ " for project " ++ show pn
                    storeNewViewInActor root source c pn v
                    loop
                StMStoreExistingView source c pn v -> do
                    logDbg $ "handling StMStoreView " ++ show (viewName v) ++ " for project " ++ show pn
                    storeExistingViewInActor root source c pn v
                    loop
                StMStop -> return ()
          logDbg t = atomically $ logDebug lch "store" t

storeNewProjectInActor :: FilePath -> SystemChan -> WAClient -> Project -> IO ()
storeNewProjectInActor root source c p = do
       mp <- storeProject root p
       atomically $ case mp of
           Nothing -> writeTChan source (SMEvent $ SEProjectStored c p)
           Just err -> writeTChan source (SMEvent $ SEProjectStoreError c p err)

storeAllProjectsInActor :: FilePath -> SystemChan -> WAClient -> AllProjects -> IO ()
storeAllProjectsInActor root source c ap = do
    mp <- storeAvailableProjects root ap
    case mp of
       Nothing -> atomically $ writeTChan source (SMEvent $ SEAllProjectsStored c ap)
       Just err -> atomically $ writeTChan source (SMEvent $ SEAllProjectsStoreError c ap err)

storeExistingProjectInActor :: FilePath -> ProjectChan -> WAClient -> Project -> IO ()
storeExistingProjectInActor root source c p = do
   mp <- storeProject root p
   case mp of
       Nothing -> atomically $ writeTChan source (PMEvent $ PEProjectStored c p)
       Just err -> atomically $ writeTChan source (PMEvent $ PEProjectStoreError c p err)


storeNewMapInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> XNamedMap ->  IO ()
storeNewMapInActor root source c pn m = do
       mp <- storeXMap root pn m
       case mp of
           Nothing -> atomically $ writeTChan source (PMEvent $ PEMapAdded c m)
           Just err -> atomically $ writeTChan source (PMEvent $ PEMapAddError c m err)

storeExistingMapInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> XNamedMap ->  IO ()
storeExistingMapInActor root source c pn m = do
       mp <- storeXMap root pn m
       case mp of
           Nothing -> atomically $ writeTChan source (PMEvent $ PEMapUpdated c m)
           Just err -> atomically $ writeTChan source (PMEvent $ PEMapUpdateError c m err)


storeNewCalculationInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> Calculation ->  IO ()
storeNewCalculationInActor root source c pn clc = do
       mp <- storeCalculation root pn clc
       case mp of
           Nothing -> atomically $ writeTChan source (PMEvent $ PECalculationAdded c clc)
           Just err -> atomically $ writeTChan source (PMEvent $ PECalculationAddError c clc err)

storeExistingCalculationInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> Calculation ->  IO ()
storeExistingCalculationInActor root source c pn clc = do
       mp <- storeCalculation root pn clc
       case mp of
           Nothing -> atomically $ writeTChan source (PMEvent $ PECalculationUpdated c clc)
           Just err -> atomically $ writeTChan source (PMEvent $ PECalculationUpdateError c clc err)


storeNewViewInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> View ->  IO ()
storeNewViewInActor root source c pn v = do
       mp <- storeView root pn v
       case mp of
           Nothing -> atomically $ writeTChan source (PMEvent $ PEViewAdded c v)
           Just err -> atomically $ writeTChan source (PMEvent $ PEViewAddError c v err)

storeExistingViewInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> View ->  IO ()
storeExistingViewInActor root source c pn v = do
       mp <- storeView root pn v
       case mp of
           Nothing -> atomically $ writeTChan source (PMEvent $ PEViewUpdated c v)
           Just err -> atomically $ writeTChan source (PMEvent $ PEViewUpdateError c v err)