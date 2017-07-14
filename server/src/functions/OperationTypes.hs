module OperationTypes where

data OperationName =
      Add
    | Subtract
    deriving (Bounded, Enum, Show, Eq, Read)
