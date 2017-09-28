module CalculationMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Errors()
import Calculation

data CalculationMessage = CMMap XNamedMap
                          | CMError Error
                          | CMUpdateCalculation Calculation
                          | CMStop
    deriving (Show, Eq)

type CalculationChan = TChan CalculationMessage
