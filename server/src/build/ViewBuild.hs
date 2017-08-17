module ViewBuild where

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

viewToChan :: LogChan -> RuntimeView -> STM ViewChan
viewToChan l v = do
        ch <- newTChan
        actorView ch v l
        return ch

viewToRuntime :: View -> STM RuntimeView
viewToRuntime v = return RuntimeView {
    view = v,
    subscribedClients = []
}

viewChansByNames :: LogChan -> [View] -> STM ViewChanByMap
viewChansByNames log v = do
        rs <- mapM viewToRuntime v
        let cs = M.fromList $ groupAssocListByKey (chanByDeps rs)
        T.mapM sequence cs
     where deps = viewDependencies . view
           chan = viewToChan log
           chansByDep vr = map (\dp -> (dp, chan vr)) (deps vr)
           chanByDeps rs = concatMap chansByDep rs