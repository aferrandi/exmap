module CalculationMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import Errors()
import Calculation
import ViewMessages

data CalculationMessage = CMMaps [XNamedMap]
                          | CMError Error
                          | CMUpdateCalculation Calculation
                          | CMViewStarted ViewChan
                          | CMStop
    deriving (Eq)

data CalculationChan = CalculationChan {
    ccChannel :: TChan CalculationMessage,
    ccName :: CalculationName
} deriving (Eq)