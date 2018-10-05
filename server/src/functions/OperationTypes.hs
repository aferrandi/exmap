module OperationTypes where

data OperationName =
      Add
    | Subtract
    | Times
    | Negate
    | Merge
    deriving (Bounded, Enum, Show, Eq, Read)

data ParameterType = ParameterDouble |
                     ParameterInt |
                     ParameterText |
                     ParameterBool |
                     ParameterAny
                deriving (Show, Eq)

data OperationType = OperationType {
    name :: OperationName,
    parametersTypes :: [ParameterType],
    returnType :: ParameterType
} deriving (Show, Eq)

newOpType :: OperationName -> [ParameterType] -> ParameterType -> OperationType
newOpType nm pts rt = OperationType { name = nm, parametersTypes = pts, returnType =  rt }

oarametersNumber :: OperationType -> Int
oarametersNumber ot = length $ parametersTypes ot

allOperationTypes :: [OperationType]
allOperationTypes = [
    newOpType Add [ParameterDouble, ParameterDouble] ParameterDouble,
    newOpType Subtract [ParameterDouble, ParameterDouble] ParameterDouble,
    newOpType Times [ParameterDouble, ParameterDouble] ParameterDouble,
    newOpType Negate [ParameterDouble] ParameterDouble,
    newOpType Merge [ParameterAny, ParameterAny] ParameterAny
    ]