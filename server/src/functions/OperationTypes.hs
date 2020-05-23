module OperationTypes where

import qualified Data.Map.Strict as M
import qualified Data.List as L

data OperationName =
      Add
    | Subtract
    | Times
    | Divide
    | Mod
    | Div
    | Negate
    | Abs
    | Sin
    | Cos
    | Tan
    | Exp
    | Log
    | GreaterThan
    | LessThan
    | GreaterOrEqual
    | LessOrEqual
    | Sum
    | Product
    | ArithmeticMean
    | GeometricMean
    | And
    | Or
    | Not
    | IfThen
    | IfThenElse
    | KeysTo
    | Merge
    | Equals
    | Len
    | ToDecimal
    | ToString
    | Trim
    | TrimLeft
    | TrimRight
    | LowerCase
    | UpperCase
    deriving (Bounded, Enum, Show, Eq, Read, Ord)

data OperationCategory =
      Math     
    | Text
    | System
    | Conversion
    | Logical
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
                     ParameterDate |
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
    newOpType (newOpId Math Divide) [ParameterDouble, ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Mod) [ParameterInt, ParameterInt] ParameterInt,
    newOpType (newOpId Math Div) [ParameterInt, ParameterInt] ParameterInt,
    newOpType (newOpId Math Negate) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Abs) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Sin) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Cos) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Tan) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Exp) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Log) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Sum) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Sum) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math Product) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math ArithmeticMean) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math GeometricMean) [ParameterDouble] ParameterDouble,
    newOpType (newOpId Math GreaterThan) [ParameterDouble] ParameterBool,
    newOpType (newOpId Math LessThan) [ParameterDouble] ParameterBool,
    newOpType (newOpId Math GreaterOrEqual) [ParameterDouble] ParameterBool,
    newOpType (newOpId Math LessOrEqual) [ParameterDouble] ParameterBool,
    newOpType (newOpId Logical And) [ParameterBool] ParameterBool,
    newOpType (newOpId Logical Or) [ParameterBool] ParameterBool,
    newOpType (newOpId Logical Not) [ParameterBool] ParameterBool,
    newOpType (newOpId Logical IfThen) [ParameterBool, ParameterAny] ParameterAny,
    newOpType (newOpId Logical IfThenElse) [ParameterBool, ParameterAny, ParameterAny] ParameterAny,
    newOpType (newOpId Conversion ToDecimal) [ParameterInt] ParameterDouble,
    newOpType (newOpId System KeysTo) [ParameterText, ParameterAny] ParameterAny,
    newOpType (newOpId System Merge) [ParameterAny, ParameterAny] ParameterAny,
    newOpType (newOpId System Equals) [ParameterAny, ParameterAny] ParameterBool,
    newOpType (newOpId Text Len) [ParameterText] ParameterInt,
    newOpType (newOpId Text ToString) [ParameterAny] ParameterText,
    newOpType (newOpId Text Trim) [ParameterText] ParameterText,
    newOpType (newOpId Text TrimLeft) [ParameterText] ParameterText,
    newOpType (newOpId Text TrimRight) [ParameterText] ParameterText,
    newOpType (newOpId Text LowerCase) [ParameterText] ParameterText,
    newOpType (newOpId Text UpperCase) [ParameterText] ParameterText
    ]

type OperationTypeByName = M.Map OperationName OperationType

operationTypeByName ::  OperationTypeByName
operationTypeByName = M.fromList $ L.map (\ot -> ((name . operationId) ot, ot) ) allOperationTypes
