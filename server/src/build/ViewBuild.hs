module ViewBuild (viewChansByNames) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent
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

viewToChan :: EventChan -> RuntimeView -> IO ViewChan
viewToChan ec v = do
        ch <- newTChanIO
        forkIO $ atomically (actorView ch v ec)
        return ch

viewToRuntime :: View -> IO RuntimeView
viewToRuntime v = do
    view <- newTVarIO v
    subscribedClients <- newTVarIO []
    return RuntimeView {
        view = view ,
        subscribedClients = subscribedClients
    }

viewChansByNames :: EventChan -> [View] -> IO ViewChanByMap
viewChansByNames evtChan vs = do
        rs <- mapM viewToRuntime vs
        cs <- mapM (viewToChan evtChan) rs
        atomically $ chansByNames evtChan (zip rs cs)

chansByNames :: EventChan -> [(RuntimeView, ViewChan)] -> STM ViewChanByMap
chansByNames evtChan rcs = do
        dc <- chanByDeps rcs
        let cs = M.fromList $ groupAssocListByKey dc
        return cs
     where chanByDeps :: [(RuntimeView,ViewChan)] -> STM [(XMapName, ViewChan)]
           chanByDeps rcs = do
                        cd <- mapM chansByDep rcs
                        return $ concat cd
           chansByDep :: (RuntimeView,ViewChan) -> STM [(XMapName, ViewChan)]
           chansByDep (vr, ch) = do
                        deps <- runtimeDependencies vr
                        return $ map (\dp -> (dp, ch)) deps


runtimeDependencies :: RuntimeView  -> STM [XMapName]
runtimeDependencies vr = do
                v <- readTVar (view vr)
                return $ viewDependencies v