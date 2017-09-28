module Calculation where

import qualified Data.Text as T

import Formula
import XMapTypes
import XFunction


newtype CalculationName = CalculationName T.Text
    deriving (Show, Eq, Ord)

newtype CalculationFormulaText = CalculationFormulaText T.Text
    deriving (Show, Eq, Ord)

data Calculation = Calculation {
    calculationName :: CalculationName,
    resultName :: XMapName,
    formula :: XFormula,
    operationMode :: OperationMode
} deriving (Show, Eq)