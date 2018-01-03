module OperationTypes where

data OperationName =
      Add
    | Subtract
    | Times
    deriving (Bounded, Enum, Show, Eq, Read)
