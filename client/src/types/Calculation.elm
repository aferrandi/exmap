module Calculation exposing (..)

import XMapTypes exposing (..)

type OperationMode = Union | Intersection

type alias CalculationName = String

type alias CalculationFormulaText = String

type alias CalculationSource = {
    calculationName : CalculationName,
    resultName : XMapName,
    formulaText : CalculationFormulaText,
    operationMode : OperationMode}