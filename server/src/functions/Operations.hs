module Operations(OperationName(..), OperationFun, operationRepository) where

import XFunction
import XMapTypes
import OperationTypes

type OperationFun = OperationMode -> XMap -> XMap -> XMapErr

addo :: OperationMode -> XMap -> XMap -> XMapErr
addo om = operate om add
    where add :: Double -> Double -> Double
          add = (+)

subtracto :: OperationMode -> XMap -> XMap -> XMapErr
subtracto om = operate om subtrct
    where subtrct :: Double -> Double -> Double
          subtrct = (-)

timeso :: OperationMode -> XMap -> XMap -> XMapErr
timeso om = operate om times
    where times :: Double -> Double -> Double
          times = (*)



operationRepository :: OperationName -> OperationFun
operationRepository Add = addo
operationRepository Subtract = subtracto
operationRepository Times = timeso
