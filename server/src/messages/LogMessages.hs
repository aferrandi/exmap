module LogMessages where

import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM
import qualified Data.Text as T

import Errors

data LogMessage = LogMErr String Error
                  | LogMDebug String T.Text
                  | LogMStop
    deriving (Eq, Show)

type LogChan = TChan LogMessage

logDebug :: LogChan -> String -> String -> STM()
logDebug logChan source t = writeTChan logChan $ LogMDebug source (T.pack t)

logError :: LogChan -> String -> Error -> STM()
logError logChan source e = writeTChan logChan $ LogMErr source e