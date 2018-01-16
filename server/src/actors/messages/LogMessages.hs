module LogMessages where

import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM
import qualified Data.Text as T

import Errors

data LogMessage = LogMErr Error
                  | LogMDebug T.Text
                  | LogMStop
    deriving (Eq, Show)

type LogChan = TChan LogMessage

logDebug :: LogChan -> String -> STM()
logDebug logChan t = writeTChan logChan $ LogMDebug (T.pack t)

logError :: LogChan -> Error -> STM()
logError logChan e = writeTChan logChan $ LogMErr e