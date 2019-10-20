{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProjectActorEvents where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as B

import TChans
import ProjectState
import ProjectMessages
import ViewMessages
import EventMessages
import WebMessages
import StoreMessages
import CommonChannels
import CalculationMessages
import WebClients
import XMapTypes
import XValues (size)
import Project
import ProjectActorEventsForCalculation
import ProjectActorEventsForView

handleEvent :: ProjectChan -> RuntimeProject -> ProjectEvent -> IO ()
handleEvent chan rp e = case e of
    PECalculationForClientLoaded c cc -> atomically $ calculationForClientLoaded rp c cc
    PECalculationForClientLoadError c _ err -> atomically $ sendError ec [c] err
    PECalculationStored c cc -> calculationStored chan rp c cc
    PECalculationStoreError c _ err -> atomically $ sendError ec [c] err
    PEMapForClientLoaded c m -> atomically $ mapForClientLoaded rp c m
    PEMapForClientLoadError c _ err -> atomically $ sendError ec [c] err
    PEMapsForViewLoaded c vn ms -> atomically $ mapsForViewLoaded rp c vn ms
    PEMapsForViewLoadError c _ _ err -> atomically $ sendError ec [c] err
    PEMapsForCalculationsLoaded c ms -> atomically $ mapsForCalculationsLoaded rp c ms
    PEMapsForCalculationsLoadError c  _ err -> atomically $ sendError ec [c] err
    PEMapsForCalculationLoaded c cn ms -> atomically $ mapsForCalculationLoaded rp c cn ms
    PEMapsForCalculationLoadError c _  _ err -> atomically $ sendError ec [c] err
    PEMapStored c m -> atomically $ mapStored chan rp c m
    PEMapStoreError c _ err -> atomically $ sendError ec [c] err
    PEProjectStored _ p -> atomically $ projectStored rp p
    PEProjectStoreError c _ err -> atomically $ sendError ec [c] err
    PEViewForClientLoaded c v -> atomically $ viewForClientLoaded c rp v
    PEViewForClientLoadError c _ err -> atomically $ sendError ec [c] err
    PEViewStored c v -> viewStored chan rp c v
    PEViewStoreError c _ err -> atomically $ sendError ec [c] err
    PEViewForProjectLoaded c v -> viewForProjectLoaded chan rp c v
    PEViewForProjectLoadError c _ err -> atomically $ sendError ec [c] err
    where ec = evtChan rp

mapForClientLoaded :: RuntimeProject -> WAClient -> XNamedMap -> STM ()
mapForClientLoaded rp c m = do
     pn <- prjName rp
     writeTChan (evtChan rp) (EMWebEvent [c] $ WEMapLoaded pn m)

newSource :: SourceType -> [XMapName] -> Source
newSource st mns = Source { sourceType = st, sourceOfMaps = mns }

mapStored :: ProjectChan -> RuntimeProject -> WAClient -> XNamedMap -> STM ()
mapStored chan rp c m = do
        let mn = xmapName m
        modifyTVar (project rp) (updateProjectWithFileMap mn)
        p <- readTVar (project rp)
        writeTChan (storeChan $ chans rp) (StMStoreExistingProject chan c p)
        sendToAllCalculations mn
        sendToAllViews mn
        let pn = projectName p
        writeTChan (evtChan rp) (EMWebEvent [c] $ WEMapStored pn mn (size $ xmap m))
        where sendToCalculations cs =
                    sendToAll (map ccChannel cs) (CMMaps [m])
              sendToAllCalculations mn = do
                    cbm <- readTVar $ calculationChanByMap rp
                    mapM_ sendToCalculations (M.lookup mn cbm)
              sendToViews vs =
                    sendToAll (map vcChannel vs) (VMMaps [m])
              sendToAllViews mn = do
                    vbm <- readTVar $ viewChanByMap rp
                    mapM_ sendToViews (M.lookup mn vbm)

projectStored :: RuntimeProject -> Project -> STM()
projectStored rp p = do
    writeTVar (project rp) p
    cs <- readTVar $ subscribedClients rp
    writeTChan (evtChan rp) (EMWebEvent cs $ WEProjectStored p)

updateProjectWithFileMap :: XMapName -> Project -> Project
updateProjectWithFileMap mn p = p { sources = updateFileSources : notFileSources }
    where isFileSources s = sourceType s == FileSource
          notFileSources = filter (not . isFileSources) (sources p)
          updateFileSources = B.maybe (newSource FileSource [mn]) updateFileSourcesContent findFileSources
          findFileSources = L.find isFileSources (sources p)
          updateFileSourcesContent s = s { sourceOfMaps = updateMaps (sourceOfMaps s)}
          updateMaps mns = L.union mns [mn]

