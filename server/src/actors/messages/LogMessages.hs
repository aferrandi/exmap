module LogMessages where

import Control.Concurrent.STM.TChan (TChan)

import Errors

data LogMessage = LogMLog Error
                  | LogMStop

type LogChan = TChan LogMessage
