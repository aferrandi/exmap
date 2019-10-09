module Types.Calculation exposing (..)

import Types.XMapTypes exposing (..)

type ParameterType
    = ParameterDouble
    | ParameterInt
    | ParameterString
    | ParameterBool
    | ParameterAny

type OperationMode
    = Union
    | Intersection

type alias CalculationName = String

type alias CalculationFormulaText = String

type alias CalculationSource =
    { calculationName : CalculationName
    , resultName : XMapName
    , formulaText : CalculationFormulaText
    , operationMode : OperationMode
    }

type alias OperationCategory = String

type alias OperationName = String

type alias OperationId =
    { category : OperationCategory,
      name : OperationName
    }

operationIdToTuple : OperationId -> (OperationCategory, OperationName)
operationIdToTuple id = (id.category, id.name)


type alias OperationType =
    { operationId : OperationId
    , parametersTypes : List ParameterType
    , returnType : ParameterType
    }

type alias Functions =
    { operations : List OperationType
    }