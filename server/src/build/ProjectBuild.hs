module ProjectBuild (projectToRuntime) where

import qualified Data.Traversable as T
import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import AssocList
import CalculationMessages
import ProjectState
import CommonChannels
import Project
import Calculation
import Dependencies
import CalculationBuild
import XMapTypes
import LogMessages

data CalculationWithChan = CalculationWithChan {
    calculation :: Calculation,
    chan :: CalculationChan
    }

type CalculationByName = M.Map CalculationName CalculationWithChan
type CalculationsByMap = M.Map XMapName [CalculationWithChan]
type CalculationByResult = M.Map XMapName CalculationWithChan


projectToRuntime :: CommonChans -> Project -> [Calculation]-> IO RuntimeProject
projectToRuntime chs p cs = do
        ccs <- T.mapM (buildCalculationChan (logChan chs)) cs
        cbn <- calculationByName ccs
        let cbm = calculationsByMap ccs
        let cbr = calculationsByResults ccs
        atomically $ do
            updateCalculationsWithDependentCalculations cbr cbm
            buildRuntimeProject chs p cbn cbm cbr
    where name = calculationName . calculation


calculationByName :: [CalculationWithChan] -> IO CalculationByName
calculationByName ccs = do
      let cbn = map (\cc -> ((calculationName . calculation) cc, cc)) ccs
      return $ M.fromList cbn

updateCalculationsWithDependentCalculations :: CalculationByResult -> CalculationsByMap->  STM ()
updateCalculationsWithDependentCalculations cbr cbm =
    mapM_ (\(rs, c) -> updateDependentCalculations rs c) $ M.toList cbr
    where
      allCalculationChannelsToNotifyForOneResult rs = fmap (\cs -> map chan cs) (M.lookup rs cbm)
      updateDependentCalculations rs c =  mapM_ (\e -> writeTChan (ccChannel $ chan c) $ CMUpdateCalculationsToNotify e) $ allCalculationChannelsToNotifyForOneResult rs

buildCalculationChan :: LogChan -> Calculation -> IO CalculationWithChan
buildCalculationChan lc c = do
    cch <- calculationToChan lc c
    return CalculationWithChan { calculation = c, chan = cch }

calculationsByMap :: [CalculationWithChan] ->  CalculationsByMap
calculationsByMap ccs = M.fromList $ groupAssocListByKey (chanByDeps ccs)
    where deps :: CalculationWithChan -> [XMapName]
          deps = calculationDependenciesMaps . calculation
          chansByDep :: CalculationWithChan -> [(XMapName, CalculationWithChan)]
          chansByDep cc = map (\dep -> (dep, cc)) (deps cc)
          chanByDeps :: [CalculationWithChan] -> [(XMapName, CalculationWithChan)]
          chanByDeps = concatMap chansByDep

calculationsByResults :: [CalculationWithChan] -> CalculationByResult
calculationsByResults cs = M.fromList $ map (\c -> (resultName $ calculation c, c)) cs

buildRuntimeProject :: CommonChans -> Project -> CalculationByName -> CalculationsByMap -> CalculationByResult -> STM RuntimeProject
buildRuntimeProject chs p cbn cbm cbr = do
    tcbn <- newTVar (M.map chan cbn)
    tcbm <- newTVar (M.map (\cs -> map chan cs) cbm)
    trp <- newTVar p
    tcbr <- newTVar (M.map (\c ->calculationName $ calculation c) cbr)
    tvbm <- newTVar M.empty
    tvbn <- newTVar M.empty
    tsc <- newTVar []
    return RuntimeProject {
        project = trp,
        calculationChanByName = tcbn,
        calculationChanByMap = tcbm,
        calculationByResult = tcbr,
        viewChanByMap = tvbm,
        viewChanByName = tvbn,
        chans = chs,
        subscribedClients = tsc
    }

