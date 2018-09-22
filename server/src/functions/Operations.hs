module Operations(OperationName(..), OperationFun, operationRepository) where

import XFunction
import XMapTypes
import OperationTypes

type OperationFun = OperationMode -> [XMap] -> XMapErr

oAdd :: OperationMode -> [XMap] -> XMapErr
oAdd om = operate om add
    where add :: Double -> Double -> Double
          add = (+)

oSubtract :: OperationMode -> [XMap] -> XMapErr
oSubtract om = operate om subtrct
    where subtrct :: Double -> Double -> Double
          subtrct = (-)

oTimes :: OperationMode -> [XMap] -> XMapErr
oTimes om = operate om times
    where times :: Double -> Double -> Double
          times = (*)

oNegate :: OperationMode -> [XMap] -> XMapErr
oNegate om = XFunction.apply negatev
    where negatev :: Double -> Double
          negatev = Prelude.negate


operationRepository :: OperationName -> OperationFun
operationRepository Add = oAdd
operationRepository Subtract = oSubtract
operationRepository Times = oTimes
operationRepository Negate = oNegate