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
import ProjectJson
import Load
import CalculationActor
import Dependencies
import CalculationBuild
import Errors
import XMapTypes

-- vertical on the screen
data CalculationWithChan = CalculationWithChan {
    calculation :: Calculation,
    chan :: CalculationChan
    }

projectToRuntime :: CommonChans -> Project -> [Calculation]-> IO RuntimeProject
projectToRuntime chans p cs = do
        ccs <- T.mapM buildCalculationChan cs
        cbn <- calculationByName ccs
        let cbm = calculationChansByNames ccs
        atomically $ buildRuntimeProject chans p cbn cbm
    where name = calculationName . calculation
          calculationByName :: [CalculationWithChan] -> IO CalculationChanByName
          calculationByName ccs = do
                let cbn = map (\cc -> (name cc, chan cc)) ccs
                return $ M.fromList cbn

buildCalculationChan :: Calculation -> IO CalculationWithChan
buildCalculationChan c = do
    cr <- atomically $ calculationToRuntime c
    cch <- calculationToChan cr
    return CalculationWithChan { calculation = c, chan = cch }

buildRuntimeProject :: CommonChans -> Project -> CalculationChanByName -> CalculationChanByMap -> STM RuntimeProject
buildRuntimeProject chans p cbn cbm = do
    tcbn <- newTVar cbn
    tcbm <- newTVar cbm
    trp <- newTVar p
    tvbm <- newTVar M.empty
    tvbn <- newTVar $ M.fromList (map (\vn -> (vn, Nothing)) (views p))
    tsc <- newTVar []
    return RuntimeProject {
        project = trp,
        calculationChanByName = tcbn,
        calculationChanByMap = tcbm,
        viewChanByMap = tvbm,
        viewChanByName = tvbn,
        chans = chans,
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
