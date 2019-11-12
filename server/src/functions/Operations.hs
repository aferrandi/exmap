{-# LANGUAGE OverloadedStrings   #-}

module Operations(OperationName(..), OperationFun, operationRepository) where

import XFunction
import XMapTypes
import XValues
import OperationTypes
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Maybe as B
import qualified Data.List as L
import qualified Data.Set as S

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
flog om xs = do
          checkMapsNumber xs 1
          ms <- extractMapDouble (head xs) "values"
          let os = M.map Prelude.log (M.filter (\v -> v >= 0) ms)
          return $ XMapDouble os

keysTo :: OperationMode -> [XMap] -> XMapErr
keysTo om xs =
        do
              checkMapsNumber xs 2
              let xv = (head (tail xs))
              mk <- extractMapString (head xs) "keys"
              return $ replaceKeys mk xv
        where
              replaceKey ks k = B.fromMaybe k $ fmap XMapKey (M.lookup k ks)
              keysInBoth vs ks = restrictMapKeys (M.keysSet ks) vs
              replaceKeysSimple ks vs = mapMapKeys (\k -> replaceKey ks k) vs
              replaceKeys ks vs = case om of
                              Intersection -> replaceKeysSimple ks (keysInBoth vs ks)
                              Union -> replaceKeysSimple ks vs

merge :: OperationMode -> [XMap] -> XMapErr
merge _ xs = do
                ms <- toMapList xs
                return $ mergeList ms
    where mergeList (XMapDoubleList xs) = XMapDouble $ M.unions xs
          mergeList (XMapIntList xs) = XMapInt $ M.unions xs
          mergeList (XMapStringList xs) = XMapString $ M.unions xs
          mergeList (XMapBoolList xs) = XMapBool $ M.unions xs
          mergeList (XMapDateList xs) = XMapDate $ M.unions xs

fsum :: OperationMode -> [XMap] -> XMapErr
fsum om xs = do
              checkMapsNumber xs 1
              vs <- extractMapDouble (head xs) "values"
              let sum = L.sum $ M.elems vs
              return $ XMapDouble (M.singleton (XMapKey "sum") sum)

equals :: OperationMode -> [XMap] -> XMapErr
equals om xs = do
     checkMapsNumber xs 2
     ms <- toMapList xs
     Right $ equalList ms
     where
          second l = (head $ tail l)
          equalList (XMapDoubleList ms) = XMapBool $ unionWith3 (==) (head ms) (second ms)
          equalList (XMapIntList ms) = XMapBool $ unionWith3 (==) (head ms) (second ms)
          equalList (XMapStringList ms) = XMapBool $ unionWith3 (==) (head ms) (second ms)
          equalList (XMapBoolList ms) = XMapBool $ unionWith3 (==) (head ms) (second ms)
          equalList (XMapDateList ms) =XMapBool $ unionWith3 (==) (head ms) (second ms)

len :: OperationMode -> [XMap] -> XMapErr
len om = XFunction.apply lenv
    where lenv :: T.Text -> Int
          lenv = T.length

toDecimal :: OperationMode -> [XMap] -> XMapErr
toDecimal om = XFunction.apply toDecimalv
    where toDecimalv :: Int -> Double
          toDecimalv = fromIntegral



operationRepository :: OperationName -> OperationFun
operationRepository op = case op of
    Add -> add
    Subtract -> fsubtract
    Times -> times
    Negate -> fnegate
    Sin -> fsin
    Cos -> fcos
    Tan -> ftan
    Exp -> fexp
    Log -> flog
    Sum -> fsum
    KeysTo -> keysTo
    Merge -> merge
    Equals -> equals
    Len -> len
    ToDecimal -> toDecimal