module OperationTypes where

import XMapTypes

data OperationName =
      Add
    | Subtract
    | Times
    | Negate
    deriving (Bounded, Enum, Show, Eq, Read)

data OperationType = OperationType {
    parametersTypes :: [XType],
    returnType :: XType
} deriving (Show, Eq)

newOpType :: [XType] -> XType -> OperationType
newOpType pts rt = OperationType { parametersTypes = pts, returnType =  rt }

oarametersNumber :: OperationType -> Int
oarametersNumber ot = length $ parametersTypes ot

operationType :: OperationName -> OperationType
operationType Add = newOpType  [XTypeDouble, XTypeDouble] XTypeDouble
operationType Subtract = newOpType  [XTypeDouble, XTypeDouble] XTypeDouble
operationType Times = newOpType  [XTypeDouble, XTypeDouble] XTypeDouble
operationType Negate = newOpType [XTypeDouble] XTypeDouble