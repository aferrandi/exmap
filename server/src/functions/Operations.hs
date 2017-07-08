module Operations(OperationName(..), OperationFun, operationRepository) where

import XFunction
import XMapTypes
import XValues

type OperationFun = OperationMode -> XMap -> XMap -> XMapErr

data OperationName =
      Add
    | Subtract
    deriving (Bounded, Enum, Show, Eq, Read)


add :: OperationMode -> XMap -> XMap -> XMapErr
add om = operate om add
    where add :: Double -> Double -> Double
          add = (+)

subtracto :: OperationMode -> XMap -> XMap -> XMapErr
subtracto om = operate om subtract
    where subtract :: Double -> Double -> Double
          subtract = (-)


operationRepository :: OperationName -> OperationFun
operationRepository Add = add
operationRepository Subtract = subtracto
