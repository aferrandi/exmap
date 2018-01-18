{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module LogActor (actorLog) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.Text as T
import System.Log.FastLogger


import Errors
import LogMessages
import Paths (logPath)

actorLog :: FilePath -> LogChan -> IO ()
actorLog root chan = do
    fileLog <- newFileLoggerSet defaultBufSize (logPath root)
    consoleLog <- newStderrLoggerSet defaultBufSize
    logLoop fileLog consoleLog chan


data TypeLog = LogDebug | LogError

instance Show TypeLog where
    show LogDebug = "DEBUG "
    show LogError = "ERROR "

logLoop :: LoggerSet -> LoggerSet -> LogChan -> IO ()
logLoop fileLog consoleLog chan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                LogMErr source (Error m) -> do
                    let txt = mkLogStr LogError source  m
                    pushLogStrLn fileLog txt
                    pushLogStrLn consoleLog txt
                    loop
                LogMDebug source s-> do
                    let txt = mkLogStr LogDebug source s
                    pushLogStrLn fileLog txt
                    loop
                LogMStop -> return ()

mkLogStr :: TypeLog -> String -> T.Text -> LogStr
mkLogStr typ src s = toLogStr $ T.append (T.pack $ show typ ++ src ++":") s

