module StateBuild (startSystem, startProject) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.List as L
import qualified Data.Traversable as T

import XMapTypes
import XFunction
import Formula
import Operations
import FormulaParser
import View
import State
import Project
import ProjectJson
import Load
import CalculationActor
import ViewActor
import LogActor
import Dependencies

mapWithNothingValues :: Ord k => [k] ->  M.Map k (Maybe a)
mapWithNothingValues = M.fromList . map (\p -> (p, Nothing))

startSystem :: FilePath -> IO RuntimeSystem
startSystem root = do
    ps <- loadSystem root
    atomically $ systemToRuntime root ps

systemToRuntime :: FilePath -> [ProjectName] -> STM RuntimeSystem
systemToRuntime root ps = do
    let psMap = mapWithNothingValues ps
    emptyByName <- newTVar psMap
    emptyByMapName <- newTVar M.empty
    log <- newTChan
    return RuntimeSystem {
        projectByName = emptyByName,
        projectByMapName = emptyByMapName,
        logForProjects = log,
        root = root
    }


loadSystem :: FilePath -> IO [ProjectName]
loadSystem root = do
    pe <- loadAvailableProjects root
    case pe of
        Right (AllProjects ps) -> return ps
        Left err -> return []

calculationToRuntime :: Calculation -> STM RuntimeCalculation
calculationToRuntime c = do
    rp <- newTVar (mapWithNothingValues deps)
    return RuntimeCalculation {
        calculation = c,
        repository = rp,
        calculationsToNotify = [],
        viewsToNotify = []
    }
    where deps = formulaDependencies (formula c)

viewToRuntime :: View -> STM RuntimeView
viewToRuntime v = return RuntimeView {
    view = v
}

calculationToChan :: RuntimeCalculation -> STM CalculationChan
calculationToChan c = do
        ch <- newTChan
        actorCalculation ch c
        return ch

viewToChan :: LogChan -> RuntimeView -> STM ViewChan
viewToChan l v = do
        ch <- newTChan
        actorView ch v l
        return ch

groupMapListByKey :: Eq k => [(k, a)] -> [(k, [a])]
groupMapListByKey xs = map oneKeyManyValues $  L.groupBy equalsKey xs
    where equalsKey (k1, _) (k2, _) = k1 == k2
          oneKeyManyValues :: [(k, a)] -> (k, [a])
          oneKeyManyValues ys = (fst (L.head ys), map snd ys)

calculationChansByNames :: [Calculation] -> STM CalculationChanByMap
calculationChansByNames c = do
        rs <- mapM calculationToRuntime c
        let cs = M.fromList $ groupMapListByKey (chanByDeps rs)
        T.mapM sequence cs
    where deps = calculationDependencies . calculation
          chansByDep cr = map (\dp -> (dp, calculationToChan cr)) (deps cr)
          chanByDeps rs =  concatMap chansByDep rs

viewChansByNames :: LogChan -> [View] -> STM ViewChanByMap
viewChansByNames log v = do
        rs <- mapM viewToRuntime v
        let cs = M.fromList $ groupMapListByKey (chanByDeps rs)
        T.mapM sequence cs
     where deps = viewDependencies . view
           chan = viewToChan log
           chansByDep vr = map (\dp -> (dp, chan vr)) (deps vr)
           chanByDeps rs = concatMap chansByDep rs

projectToRuntime :: LogChan -> Project -> STM RuntimeProject
projectToRuntime log p = do
        cs <- calculationChansByNames (calculations p)
        vs <- viewChansByNames log (views p)
        csv <- newTVar cs
        vsv <- newTVar vs
        return RuntimeProject {
            project = p,
            calculationByMap = csv,
            viewByMap = vsv,
            logForViews = log
       }


startProject :: LogChan -> FilePath -> ProjectName -> IO (Either String RuntimeProject)
startProject log root n = do
    pe <- loadProject root n
    atomically $ mapM (projectToRuntime log) pe

