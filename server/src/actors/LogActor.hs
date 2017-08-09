module LogActor where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import State
import qualified Data.Text.IO as TIO

actorLog :: LogChan -> IO ()
actorLog chan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                LMLog m -> do
                    TIO.putStrLn m
                    loop
                LMStop -> return ()
