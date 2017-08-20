module LogMessages where

import Control.Concurrent.STM.TChan (TChan)

import Errors

data LogMessage = LMLog Error
                  | LMStop

type LogChan = TChan LogMessage
