{-# LANGUAGE OverloadedStrings   #-}

module Operations(OperationName(..), OperationFun, operationRepository) where

import XFunction
import XMapTypes
import XValues
import OperationTypes
import qualified Data.Map.Strict as M

type OperationFun = OperationMode -> [XMap] -> XMapErr

add :: OperationMode -> [XMap] -> XMapErr
add om = operate om addv
    where addv :: Double -> Double -> Double
          addv = (+)

fsubtract :: OperationMode -> [XMap] -> XMapErr
fsubtract om = operate om subtractv
    where subtractv :: Double -> Double -> Double
          subtractv = (-)

times :: OperationMode -> [XMap] -> XMapErr
times om = operate om timesv
    where timesv :: Double -> Double -> Double
          timesv = (*)

fnegate :: OperationMode -> [XMap] -> XMapErr
fnegate om = XFunction.apply negatev
    where negatev :: Double -> Double
          negatev = Prelude.negate

merge :: OperationMode -> [XMap] -> XMapErr
merge _ xs = do
                ms <- toMapList xs
                return $ mergeList ms
    where mergeList (XMapDoubleList xs) = XMapDouble $ M.unions xs
          mergeList (XMapIntList xs) = XMapInt $ M.unions xs
          mergeList (XMapStringList xs) = XMapString $ M.unions xs
          mergeList (XMapBoolList xs) = XMapBool $ M.unions xs

operationRepository :: OperationName -> OperationFun
operationRepository Add = add
operationRepository Subtract = fsubtract
operationRepository Times = times
operationRepository Negate = fnegate
operationRepository Merge = merge