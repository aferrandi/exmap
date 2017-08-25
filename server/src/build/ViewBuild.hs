module ViewBuild where

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
import Project

viewToChan :: EventChan -> RuntimeView -> IO ViewChan
viewToChan ec v = do
        ch <- newTChanIO
        forkIO $ atomically (actorView ch v ec)
        return ch

viewToRuntime :: ProjectName -> View -> STM RuntimeView
viewToRuntime pn v = do
    view <- newTVar v
    subscribedClients <- newTVar []
    status <- newTVar M.empty
    return RuntimeView {
        runtimeViewName = viewName v,
        view = view ,
        subscribedClients = subscribedClients,
        ownerProjectName = pn,
        status = status
    }


runtimeDependencies :: RuntimeView  -> STM [XMapName]
runtimeDependencies vr = do
                v <- readTVar (view vr)
                return $ viewDependencies v