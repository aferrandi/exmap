module ViewActor(actorView) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import ViewState
import ProjectState
import ActorMessages

actorView :: ViewChan -> RuntimeView -> LogChan -> STM ()
actorView chan rtView logChan = loop
    where loop = do
            msg <- readTChan chan
            case msg of
                VMMap m -> do

                    loop
                VMLog t -> do
                    writeTChan logChan (LMLog t)
                    loop
                VMStop -> return ()



