module CalculationMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Errors()
import Calculation
import ViewMessages

data CalculationMessage = CMMap XNamedMap
                          | CMError Error
                          | CMUpdateCalculation Calculation
                          | CMViewStarted ViewChan
                          | CMStop
    deriving (Eq)

type CalculationChan = TChan CalculationMessage
