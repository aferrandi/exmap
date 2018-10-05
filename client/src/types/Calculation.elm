module Calculation exposing (..)

import XMapTypes exposing (..)

type ParameterType = ParameterDouble
                | ParameterInt
                | ParameterString
                | ParameterBool
                | ParameterAny

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
    parametersTypes : List ParameterType,
    returnType : ParameterType
    }

type alias Functions = {
    operations : List OperationType
    }
