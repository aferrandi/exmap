{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProjectActorRequests where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.List as L
import qualified Data.Map.Strict as M

import ProjectState
import CheckProjectContent
import ProjectMessages
import ViewMessages
import EventMessages
import WebMessages
import LoadMessages
import StoreMessages
import CommonChannels
import WebClients
import XMapTypes
import View
import Project
import Calculation
import FormulaParser
import Formula

handleRequests:: ProjectChan -> RuntimeProject -> ProjectRequest -> STM ()
handleRequests chan rp r= case r of
    PRSubscribeToProject c -> subscribeToProject rp c
    PRUnsubscribeFromProject c -> removeSubscriber rp c
    PRSubscribeToView c vn -> subscribeToView chan rp c vn
    PRUnsubscribeFromView c vn -> unsubscribeFromView c vn rp
    PRMapsInProject c -> mapsInProject rp c
    PRUpdateProject c p -> updateProject chan rp c p
    PRLoadMapForClient c mn -> loadMapForClient c chan rp mn
    PRAddMap c m -> addMap c chan rp m
    PRUpdateMap c m -> updateMap c chan rp m
    PRLoadCalculationForClient c cn -> loadCalculation chan rp c cn
    PRAddCalculation c cs-> addCalculation chan rp c cs
    PRUpdateCalculation c cs -> updateCalculation chan rp c cs
    PRLoadViewForClient c vn -> loadView chan rp c vn
    PRAddView c v -> addView chan rp c v
    PRUpdateView c v -> updateView chan rp c v
    PRStartCalculations c -> startCalculations chan rp c
    PRDisconnect c -> disconnectClient rp c

loadMapForClient :: WAClient -> ProjectChan -> RuntimeProject -> XMapName -> STM ()
loadMapForClient c chan rp mn = do
     pn <- prjName rp
     writeTChan (loadChan $ chans rp)  $ LMLoadMapForClient chan c pn mn

addMap :: WAClient -> ProjectChan -> RuntimeProject -> XNamedMap -> STM ()
addMap c chan rp m = do
    pn <- prjName rp
    let mn = xmapName m 
    fndMap <- projectContainsMapWithName rp mn
    fndResult <- projectContainsResultWithName rp mn
    if not (fndMap || fndResult) then do
      writeTChan (storeChan $ chans rp)  $ StMStoreNewMap chan c pn m
    else
      sendStringError  (evtChan rp) [c] $ "A map with name " ++ (show mn) ++ " already exists"

updateMap :: WAClient -> ProjectChan -> RuntimeProject -> XNamedMap -> STM ()
updateMap c chan rp m = do
    pn <- prjName rp
    let mn = xmapName m
    fnd <- projectContainsMapWithName rp mn
    if fnd then do
      writeTChan (storeChan $ chans rp)  $ StMStoreExistingMap chan c pn m
    else
      sendStringError  (evtChan rp) [c] $ "A map with name " ++ (show mn) ++ " to update does'nt exists"

disconnectClient :: RuntimeProject -> WAClient -> STM()
disconnectClient rp c = do
    removeSubscriber rp c
    disconnectAll
    where disconnectAll = do
           vs <- readTVar $ viewChanByName rp
           mapM_ (\vc -> writeTChan (vcChannel vc) (VMUnsubscribeFromView c)) (M.elems vs)

subscribeToProject :: RuntimeProject -> WAClient -> STM()
subscribeToProject rp c = do
    p <- readTVar $ project rp
    addSubscriber rp c
    writeTChan (evtChan rp) $ EMWebEvent [c] (WEProjectContent p)

removeSubscriber :: RuntimeProject -> WAClient -> STM()
removeSubscriber rp c = modifyTVar (subscribedClients rp) (filter notSameClient)
    where notSameClient ci = ci /= c

mapsInProject :: RuntimeProject -> WAClient -> STM ()
mapsInProject rp c = do
    p <- readTVar (project rp)
    cbm <- readTVar $ calculationChanByMap rp
    cbr <- readTVar $ calculationByResult rp
    let ss = L.nub (concatMap sourceOfMaps (sources p) ++ M.keys cbm ++ M.keys cbr)
    writeTChan (evtChan rp) $ EMWebEvent [c] (WEMapsInProject (projectName p) ss)

subscribeToView :: ProjectChan -> RuntimeProject -> WAClient -> ViewName -> STM ()
subscribeToView chan rp c vn = do
    vs <- readTVar $ viewChanByName rp
    pn <- prjName rp
    case M.lookup vn vs of
        Just vChan -> writeTChan (vcChannel vChan) (VMSubscribeToView c)
        Nothing -> writeTChan (loadChan $ chans rp) (LMLoadViewForProject chan c pn vn)

unsubscribeFromView :: WAClient -> ViewName -> RuntimeProject -> STM ()
unsubscribeFromView c vn rp = do
    vs <- readTVar $ viewChanByName rp
    pn <- prjName rp
    case M.lookup vn vs of
        Just vChan -> writeTChan (vcChannel vChan) (VMUnsubscribeFromView c)
        Nothing -> sendStringError  (evtChan rp) [c] ("view " ++ show vn ++ " to unsubscribe from not found in project " ++ show pn)

updateProject :: ProjectChan -> RuntimeProject -> WAClient -> Project -> STM ()
updateProject chan rp c p =
    writeTChan (storeChan $ chans rp) (StMStoreExistingProject chan c p)
    -- update calculations and views

loadCalculation :: ProjectChan -> RuntimeProject -> WAClient -> CalculationName -> STM ()
loadCalculation chan rp c cn = do
     pn <- prjName rp
     writeTChan (loadChan $ chans rp)  $ LMLoadCalculation chan c pn cn

addCalculation :: ProjectChan -> RuntimeProject -> WAClient -> CalculationSource -> STM ()
addCalculation chan rp c cs = do
    let rn = sourceResultName cs
    let cn = sourceCalculationName cs
    pn <- prjName rp
    fndMap <- projectContainsMapWithName rp rn
    fndResult <- projectContainsResultWithName rp rn
    if not (fndMap || fndResult) then do
      fndCalculation <- projectContainsCalculationWithName rp cn
      if not fndCalculation then do
        let ft = formulaText cs
        case parseFormula ft of
            Left err -> sendStringError  (evtChan rp) [c] ("Parsing the formula " ++ show ft ++ " got " ++ show err)
            Right f -> do
                let cc = calculationFromFormula cs f
                writeTChan (storeChan $ chans rp)  $ StMStoreNewCalculation chan c pn cc
      else
        sendStringError  (evtChan rp) [c] $ "A calculation with name" ++ (show cn) ++ " already exists in the project" ++ (show pn)
    else
      sendStringError  (evtChan rp) [c] $ "A map with the name of the result " ++ (show rn) ++ " of the calculation " ++ (show cn) ++ " already exists in the project" ++ (show pn)

updateCalculation :: ProjectChan -> RuntimeProject -> WAClient -> CalculationSource -> STM ()
updateCalculation chan rp c cs = do
    let cn = sourceCalculationName cs
    pn <- prjName rp
    fnd <- projectContainsCalculationWithName rp cn
    if fnd then do
      let ft = formulaText cs
      case parseFormula ft of
          Left err -> sendStringError  (evtChan rp) [c] ("Parsing the formula " ++ show ft ++ " got " ++ show err)
          Right f -> do
              let cc = calculationFromFormula cs f
              writeTChan (storeChan $ chans rp)  $ StMStoreExistingCalculation chan c pn cc
    else
      sendStringError  (evtChan rp) [c] $ "The calculation with name " ++ (show cn) ++ " to update does not exist in the project" ++ (show pn)



addView :: ProjectChan -> RuntimeProject -> WAClient -> View -> STM ()
addView chan rp c v = do
    pn <- prjName rp
    fnd <- projectContainsViewWithName rp (viewName v)
    if not fnd then do
      writeTChan (storeChan $ chans rp)  $ StMStoreNewView chan c pn v
    else
      sendStringError  (evtChan rp) [c] $ "A view with name " ++ (show $ viewName v) ++ " already exists in the project" ++ (show pn)

updateView :: ProjectChan -> RuntimeProject -> WAClient -> View -> STM ()
updateView chan rp c v = do
    pn <- prjName rp
    fnd <- projectContainsViewWithName rp (viewName v)
    if fnd then do
      writeTChan (storeChan $ chans rp)  $ StMStoreExistingView chan c pn v
    else
      sendStringError  (evtChan rp) [c] $ "The view with name " ++ (show $ viewName v) ++ " to update does not exist in the project" ++ (show pn)

loadView :: ProjectChan -> RuntimeProject -> WAClient -> ViewName -> STM ()
loadView chan rp c vn = do
    pn <- prjName rp
    writeTChan (loadChan $ chans rp) $ LMLoadViewForClient chan c pn vn

startCalculations :: ProjectChan -> RuntimeProject -> WAClient -> STM()
startCalculations chan rp c = do
    p <- readTVar $ project rp
    case L.find (\s -> sourceType s == FileSource) (sources p) of
        Just fileSources -> do
            cbm <- readTVar (calculationChanByMap rp)
            let cms = L.intersect (M.keys cbm) (sourceOfMaps fileSources)
            writeTChan (loadChan $ chans rp) $ LMLoadMapsForCalculations chan c (projectName p) cms
        Nothing -> return ()

addSubscriber ::RuntimeProject -> WAClient -> STM ()
addSubscriber rp c= modifyTVar (subscribedClients rp) (\cs -> c : cs)

calculationFromFormula :: CalculationSource -> XFormula -> Calculation
calculationFromFormula cs f = Calculation {
                                   calculationName = sourceCalculationName cs,
                                   resultName = sourceResultName cs,
                                   formula = f,
                                   operationMode = sourceOperationMode cs
                               }
