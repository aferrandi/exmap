{-# LANGUAGE OverloadedStrings   #-}

module Operations(OperationName(..), OperationFun, operationRepository) where

import XFunction
import XMapTypes
import XValues
import OperationTypes
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Maybe as B

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

fsin :: OperationMode -> [XMap] -> XMapErr
fsin om = XFunction.apply sinv
    where sinv :: Double -> Double
          sinv = Prelude.sin

fcos :: OperationMode -> [XMap] -> XMapErr
fcos om = XFunction.apply cosv
    where cosv :: Double -> Double
          cosv = Prelude.cos

ftan :: OperationMode -> [XMap] -> XMapErr
ftan om = XFunction.apply tanv
    where tanv :: Double -> Double
          tanv = Prelude.tan

fexp :: OperationMode -> [XMap] -> XMapErr
fexp om = XFunction.apply expv
    where expv :: Double -> Double
          expv = Prelude.exp


flog :: OperationMode -> [XMap] -> XMapErr
flog om = XFunction.apply logv
    where logv :: Double -> Double
          logv = Prelude.log

keysTo :: OperationMode -> [XMap] -> XMapErr
keysTo _ xs =
    case length xs of
       2 -> keysTo
       _ -> Left $ Error "keysTo <keys> <map>"
    where
        extractKeysMap :: XMap -> Either Error (MapValue T.Text)
        extractKeysMap (XMapString x) = Right x
        extractKeysMap _ = Left $ Error "The 'keys' map must be of type text"
        replaceKey mk k = B.fromMaybe k (fmap XMapKey (M.lookup k mk))
        keysTo = do
              mk <- extractKeysMap (head xs)
              let mv = mapMapKeys (\k -> replaceKey mk k) (head (tail xs))
              return mv

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
operationRepository Sin = fsin
operationRepository Cos = fcos
operationRepository Tan = ftan
operationRepository Exp = fexp
operationRepository Log = flog
operationRepository Merge = merge