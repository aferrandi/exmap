module ViewActor(actorView) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import State

actorView :: ViewChan -> RuntimeView -> LogChan -> STM ()
actorView chan rtView logChan = loop
    where loop = do
            msg <- readTChan chan
            case msg of
                VMMap m -> do
                    loop
                VMLog t -> do
                    writeTChan logChan (LMLog t)
                    return ()
                VMStop -> return ()



