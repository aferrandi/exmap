module OperationTypes where

data OperationName =
      Add
    | Subtract
    | Times
    | Negate
    | Sin
    | Cos
    | Tan
    | Exp
    | Log
    | KeysTo
    | Merge
    deriving (Bounded, Enum, Show, Eq, Read)

data OperationCategory =
      Math
    | System
    deriving (Bounded, Enum, Show, Eq, Read)

data OperationId = OperationId {
    category :: OperationCategory,
    name :: OperationName
} deriving (Show, Eq)

newOpId :: OperationCategory -> OperationName -> OperationId
newOpId ct nm = OperationId { category = ct, name = nm }

data ParameterType = ParameterDouble |
                     ParameterInt |
                     ParameterText |
                     ParameterBool |
                     ParameterAny
                deriving (Show, Eq)

data OperationType = OperationType {
    operationId :: OperationId,
    parametersTypes :: [ParameterType],
    returnType :: ParameterType
} deriving (Show, Eq)

newOpType :: OperationId -> [ParameterType] -> ParameterType -> OperationType
newOpType id pts rt = OperationType { operationId = id, parametersTypes = pts, returnType =  rt }

oarametersNumber :: OperationType -> Int
oarametersNumber ot = length $ parametersTypes ot

allOperationTypes :: [OperationType]
allOperationTypes = [
    newOpType (newOpId Math Add) [ParameterDouble, ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Subtract) [ParameterDouble, ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Times) [ParameterDouble, ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Negate) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Sin) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Cos) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Tan) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Exp) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Log) [ParameterDouble] ParameterDouble,
    newOpType (newOpId System KeysTo) [ParameterText, ParameterAny] ParameterAny,
    newOpType (newOpId System Merge) [ParameterAny, ParameterAny] ParameterAny
    ]