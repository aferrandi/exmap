module ViewBuild (viewChansByNames) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Traversable as T

import ProjectState
import ViewState
import View
import AssocList
import ViewActor
import Dependencies
import ViewMessages
import EventMessages
import XMapTypes

viewToChan :: EventChan -> RuntimeView -> STM ViewChan
viewToChan ec v = do
        ch <- newTChan
        actorView ch v ec
        return ch

viewToRuntime :: View -> STM RuntimeView
viewToRuntime v = do
    view <- newTVar v
    subscribedClients <- newTVar []
    return RuntimeView {
        view = view ,
        subscribedClients = subscribedClients
    }

viewChansByNames :: EventChan -> [View] -> STM ViewChanByMap
viewChansByNames evtChan vs = do
        rs <- mapM viewToRuntime vs
        dc <- chanByDeps rs
        let cs = M.fromList $ groupAssocListByKey dc
        return cs
     where chanByDeps :: [RuntimeView] -> STM [(XMapName, ViewChan)]
           chanByDeps rs = do
                        cd <- mapM chansByDep rs
                        return $ concat cd
           chansByDep :: RuntimeView -> STM [(XMapName, ViewChan)]
           chansByDep vr = do
                        deps <- runtimeDependencies vr
                        ch <- viewToChan evtChan vr
                        return $ map (\dp -> (dp, ch)) deps


runtimeDependencies :: RuntimeView  -> STM [XMapName]
runtimeDependencies vr = do
                v <- readTVar (view vr)
                return $ viewDependencies v