{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module LogActor (actorLog) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Errors
import LogMessages

actorLog :: LogChan -> IO ()
actorLog chan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                LogMErr source (Error m) -> do
                    TIO.putStrLn $ T.append (T.pack $ "ERROR "++ source ++":") m
                    loop
                LogMDebug source s-> do
                    TIO.putStrLn $ T.append (T.pack $ "DEBUG "++ source ++":") s
                    loop
                LogMStop -> return ()
