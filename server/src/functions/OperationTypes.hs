module OperationTypes where

import XMapTypes

data OperationName =
      Add
    | Subtract
    | Times
    | Negate
    deriving (Bounded, Enum, Show, Eq, Read)

data OperationType = OperationType {
    name :: OperationName,
    parametersTypes :: [XMapType],
    returnType :: XMapType
} deriving (Show, Eq)

newOpType :: OperationName -> [XMapType] -> XMapType -> OperationType
newOpType nm pts rt = OperationType { name = nm, parametersTypes = pts, returnType =  rt }

oarametersNumber :: OperationType -> Int
oarametersNumber ot = length $ parametersTypes ot

allOperationTypes :: [OperationType]
allOperationTypes = [
    newOpType Add [TypeDouble, TypeDouble] TypeDouble,
    newOpType Subtract [TypeDouble, TypeDouble] TypeDouble,
    newOpType Times [TypeDouble, TypeDouble] TypeDouble,
    newOpType Negate [TypeDouble] TypeDouble
    ]