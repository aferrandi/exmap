{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module LogActor (actorLog) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.Text as T
import System.Log.FastLogger
import Data.Monoid ((<>))

import Errors
import LogMessages
import Paths (logPath)

actorLog :: FilePath -> LogChan -> IO ()
actorLog root chan = do
    timeCache <- newTimeCache simpleTimeFormat'
    (consoleLog, consoleCleanUp) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)
    (fileLog, fileCleanUp) <- newTimedFastLogger timeCache (LogFile fileSpec defaultBufSize)
    logLoop fileLog consoleLog chan
    consoleCleanUp
    fileCleanUp
    where fileSpec = FileLogSpec {
                                  log_file = logPath root,
                                  log_file_size = ((10::Integer)^(6::Integer))::Integer,
                                  log_backup_number = 4}

data TypeLog = LogDebug | LogError

instance Show TypeLog where
    show LogDebug = "DEBUG "
    show LogError = "ERROR "

logLoop :: TimedFastLogger -> TimedFastLogger -> LogChan -> IO ()
logLoop fileLog consoleLog chan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                LogMErr source (Error m) -> do
                    pushLog fileLog LogError source m
                    pushLog consoleLog LogError source m
                    loop
                LogMDebug source s-> do
                    pushLog fileLog LogDebug source s
                    loop
                LogMStop -> return ()


pushLog :: TimedFastLogger -> TypeLog -> String -> T.Text -> IO ()
pushLog logger typ src msg = logger $ \ft -> toLogStr ft <> toLogStr " " <> toLogStr (show typ) <> toLogStr src <> toLogStr ": " <> toLogStr msg <> toLogStr "\n"
