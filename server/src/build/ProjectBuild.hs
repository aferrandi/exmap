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

-- vertical on the screen
data CalculationWithChan = CalculationWithChan {
    calculation :: Calculation,
    chan :: CalculationChan
    }

projectToRuntime :: CommonChans -> Project -> [Calculation]-> IO RuntimeProject
projectToRuntime chs p cs = do
        ccs <- T.mapM (buildCalculationChan (logChan chs)) cs
        cbn <- calculationByName ccs
        let cbm = calculationChansByNames ccs
        let cbr = calculationsByResults cs
        atomically $ buildRuntimeProject chs p cbn cbm cbr
    where name = calculationName . calculation
          calculationByName :: [CalculationWithChan] -> IO CalculationChanByName
          calculationByName ccs = do
                let cbn = map (\cc -> (name cc, chan cc)) ccs
                return $ M.fromList cbn

buildCalculationChan :: LogChan -> Calculation -> IO CalculationWithChan
buildCalculationChan lc c = do
    cch <- calculationToChan lc c
    return CalculationWithChan { calculation = c, chan = cch }

buildRuntimeProject :: CommonChans -> Project -> CalculationChanByName -> CalculationChanByMap -> CalculationByResult -> STM RuntimeProject
buildRuntimeProject chs p cbn cbm cbr = do
    tcbn <- newTVar cbn
    tcbm <- newTVar cbm
    trp <- newTVar p
    tcbr <- newTVar cbr
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

calculationChansByNames :: [CalculationWithChan] ->  CalculationChanByMap
calculationChansByNames ccs = M.fromList $ groupAssocListByKey (chanByDeps ccs)
        -- return $ T.mapM sequence cs
    where deps :: CalculationWithChan -> [XMapName]
          deps = calculationDependencies . calculation
          chansByDep :: CalculationWithChan -> [(XMapName, CalculationChan)]
          chansByDep cc = map (\dep -> (dep, chan cc)) (deps cc)
          chanByDeps :: [CalculationWithChan] -> [(XMapName, CalculationChan)]
          chanByDeps = concatMap chansByDep

calculationsByResults :: [Calculation] -> CalculationByResult
calculationsByResults cs = M.fromList $ map (\c -> (resultName c, calculationName c)) cs