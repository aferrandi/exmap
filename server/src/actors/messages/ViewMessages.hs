module ViewMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Errors

data ViewMessage = VMMap XNamedMap
                   | VMError Error
                   | VMStop

type ViewChan = TChan ViewMessage
