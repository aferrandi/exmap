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
                StMStoreMap source c pn m -> do
                    logDbg $ "handling StMStoreMap " ++ show (xmapName m) ++ " for project " ++ show pn
                    storeMapInActor root source c pn m
                    loop
                StMStoreCalculation source c pn clc -> do
                    logDbg $ "handling StMStoreCalculation " ++ show (calculationName clc) ++ " for project " ++ show pn
                    storeCalculationInActor root source c pn clc
                    loop
                StMStoreView source c pn v -> do
                    logDbg $ "handling StMStoreView " ++ show (viewName v) ++ " for project " ++ show pn
                    storeViewInActor root source c pn v
                    loop
                StMStop -> return ()
          logDbg t = atomically $ logDebug lch t

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


storeMapInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> XNamedMap ->  IO ()
storeMapInActor root source c pn m = do
       mp <- storeXMap root pn m
       case mp of
           Nothing -> atomically $ writeTChan source (PMEvent $ PEMapStored c m)
           Just err -> atomically $ writeTChan source (PMEvent $ PEMapStoreError c m err)

storeCalculationInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> Calculation ->  IO ()
storeCalculationInActor root source c pn clc = do
       mp <- storeCalculation root pn clc
       case mp of
           Nothing -> atomically $ writeTChan source (PMEvent $ PECalculationStored c clc)
           Just err -> atomically $ writeTChan source (PMEvent $ PECalculationStoreError c clc err)

storeViewInActor :: FilePath -> ProjectChan -> WAClient -> ProjectName -> View ->  IO ()
storeViewInActor root source c pn v = do
       mp <- storeView root pn v
       case mp of
           Nothing -> atomically $ writeTChan source (PMEvent $ PEViewStored c v)
           Just err -> atomically $ writeTChan source (PMEvent $ PEViewStoreError c v err)