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

-- vertical on the screen
data CalculationWithChan = CalculationWithChan {
    calculation :: Calculation,
    chan :: CalculationChan
    }

projectToRuntime :: CommonChans -> Project -> [Calculation]-> IO RuntimeProject
projectToRuntime chs p cs = do
        ccs <- T.mapM buildCalculationChan cs
        cbn <- calculationByName ccs
        let cbm = calculationChansByNames ccs
        let rbn = calculationResultsByNames cs
        atomically $ buildRuntimeProject chs p cbn cbm rbn
    where name = calculationName . calculation
          calculationByName :: [CalculationWithChan] -> IO CalculationChanByName
          calculationByName ccs = do
                let cbn = map (\cc -> (name cc, chan cc)) ccs
                return $ M.fromList cbn

buildCalculationChan :: Calculation -> IO CalculationWithChan
buildCalculationChan c = do
    cch <- calculationToChan c
    return CalculationWithChan { calculation = c, chan = cch }

buildRuntimeProject :: CommonChans -> Project -> CalculationChanByName -> CalculationChanByMap -> CalculationResultByName -> STM RuntimeProject
buildRuntimeProject chs p cbn cbm rbn = do
    tcbn <- newTVar cbn
    tcbm <- newTVar cbm
    trp <- newTVar p
    trbn <- newTVar rbn
    tvbm <- newTVar M.empty
    tvbn <- newTVar M.empty
    tsc <- newTVar []
    return RuntimeProject {
        project = trp,
        calculationChanByName = tcbn,
        calculationChanByMap = tcbm,
        calculationResultByName = trbn,
        viewChanByMap = tvbm,
        viewChanByName = tvbn,
        chans = chs,
        subscribedClients = tsc
    }

calculationChansByNames :: [CalculationWithChan] ->  CalculationChanByMap
calculationChansByNames ccs = M.fromList $ groupAssocListByKey (chanByDeps ccs)
        -- return $ T.mapM sequence cs
    where deps :: CalculationWithChan -> [XMapName]
          deps = calculationDependencies . calculation
          chansByDep :: CalculationWithChan -> [(XMapName, CalculationChan)]
          chansByDep cc = map (\dep -> (dep, chan cc)) (deps cc)
          chanByDeps :: [CalculationWithChan] -> [(XMapName, CalculationChan)]
          chanByDeps = concatMap chansByDep

calculationResultsByNames :: [Calculation] -> CalculationResultByName
calculationResultsByNames cs = M.fromList $ map (\c -> (calculationName c, resultName c)) cs