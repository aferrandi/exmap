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
import LogMessages
import Project

viewToChan :: EventChan -> LogChan -> ProjectName -> View -> IO ViewChan
viewToChan ec lc pn v = do
        rv <- atomically $ viewToRuntime lc pn v
        ch <- newTChanIO
        let vch = ViewChan {
              vcChannel = ch,
              vcName = viewName v
        }
        _ <- forkIO $ actorView vch rv ec
        return vch

viewToRuntime :: LogChan -> ProjectName -> View -> STM RuntimeView
viewToRuntime lc pn v = do
    vv <- newTVar v
    cs <- newTVar []
    ms <- newTVar M.empty
    return RuntimeView {
        runtimeViewName = viewName v,
        view = vv,
        subscribedClients = cs,
        ownerProjectName = pn,
        mapsInView = ms,
        logChan = lc
    }
