module Calculation exposing (..)

import XMapTypes exposing (..)

type OperationMode = Union | Intersection

type alias CalculationName = String

type alias CalculationFormulaText = String

type alias CalculationSource = {
    calculationName : CalculationName,
    resultName : XMapName,
    formulaText : CalculationFormulaText,
    operationMode : OperationMode
    }

type alias OperationName = String

type alias OperationType = {
    name : OperationName,
    parametersTypes : List XMapType,
    returnType : XMapType
    }

type alias Functions = {
    operations : List OperationType
    }
