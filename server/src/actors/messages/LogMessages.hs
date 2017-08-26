module LogMessages where

import Control.Concurrent.STM.TChan (TChan)

import Errors

data LogMessage = LogMLog Error
                  | LogMStop
    deriving (Eq, Show)

type LogChan = TChan LogMessage
