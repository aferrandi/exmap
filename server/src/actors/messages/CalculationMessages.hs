module CalculationMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Errors

data CalculationMessage = CMMap XNamedMap
                          | CMError Error
                          | CMStop

type CalculationChan = TChan CalculationMessage
