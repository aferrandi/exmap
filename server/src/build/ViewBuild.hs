module ViewBuild where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent

import ViewState
import View
import ViewActor
import ViewMessages
import EventMessages
import Project

viewToChan :: EventChan -> ProjectName -> View -> IO ViewChan
viewToChan ec pn v = do
        rv <- atomically $ viewToRuntime pn v
        ch <- newTChanIO
        _ <- forkIO $ actorView ch rv ec
        return ch

viewToRuntime :: ProjectName -> View -> STM RuntimeView
viewToRuntime pn v = do
    vv <- newTVar v
    cs <- newTVar []
    st <- newTVar M.empty
    return RuntimeView {
        runtimeViewName = viewName v,
        view = vv,
        subscribedClients = cs,
        ownerProjectName = pn,
        status = st
    }

{-
runtimeDependencies :: RuntimeView  -> STM [XMapName]
runtimeDependencies vr = do
                v <- readTVar (view vr)
                return $ viewDependencies v
-}