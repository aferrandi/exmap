{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module LogActor (actorLog) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.Text.IO as TIO

import Errors
import LogMessages

actorLog :: LogChan -> IO ()
actorLog chan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                LogMLog (Error m) -> do
                    TIO.putStrLn m
                    loop
                LogMStop -> return ()
