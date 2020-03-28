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
import ShowText

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

divide :: OperationMode -> [XMap] -> XMapErr
divide om xs = do
    checkMapsNumber xs 2
    ms1 <- extractMapDouble (head xs) "dividend"
    ms2 <- extractMapDouble (head $ tail xs) "divisor"
    let fs2 = M.filter (\v -> v /= 0) ms2
    let fs1 = M.restrictKeys ms1 (M.keysSet ms2)
    operate om dividev [XMapDouble fs1, XMapDouble fs2]
    where dividev :: Double -> Double -> Double
          dividev = (/)

fnegate :: OperationMode -> [XMap] -> XMapErr
fnegate om = XFunction.apply negatev
    where negatev :: Double -> Double
          negatev = Prelude.negate

fabs :: OperationMode -> [XMap] -> XMapErr
fabs om = XFunction.apply absv
    where absv :: Double -> Double
          absv = Prelude.abs

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

greaterThan :: OperationMode -> [XMap] -> XMapErr
greaterThan om = operate om greaterv
    where greaterv :: Double -> Double -> Bool
          greaterv = (>)

lessThan :: OperationMode -> [XMap] -> XMapErr
lessThan om = operate om lessv
    where lessv :: Double -> Double -> Bool
          lessv = (<)

greaterOrEqual :: OperationMode -> [XMap] -> XMapErr
greaterOrEqual om = operate om greatereqv
    where greatereqv :: Double -> Double -> Bool
          greatereqv = (>=)

lessOrEqual :: OperationMode -> [XMap] -> XMapErr
lessOrEqual om = operate om lesseqv
    where lesseqv :: Double -> Double -> Bool
          lesseqv = (<=)

fand :: OperationMode -> [XMap] -> XMapErr
fand om = operate om (&&)

for :: OperationMode -> [XMap] -> XMapErr
for om = operate om (||)

fnot :: OperationMode -> [XMap] -> XMapErr
fnot om = XFunction.apply notv
    where notv ::Bool -> Bool
          notv = not
          
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

fproduct :: OperationMode -> [XMap] -> XMapErr
fproduct om xs = do
              checkMapsNumber xs 1
              vs <- extractMapDouble (head xs) "values"
              let prod = L.product $ M.elems vs
              return $ XMapDouble (M.singleton (XMapKey "product") prod)

average :: OperationMode -> [XMap] -> XMapErr
average om xs = do
              checkMapsNumber xs 1
              vs <- extractMapDouble (head xs) "values"
              let vsl = M.elems vs
              return $ XMapDouble (M.singleton (XMapKey "avg") (avg vsl))
      where avg vsl = if L.null vsl
                        then 0
                        else L.sum vsl / L.genericLength vsl
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

toString ::  OperationMode -> [XMap] -> XMapErr
toString om xs = Right $ toStringv (head xs)
    where toStringv (XMapDouble ms) = XMapString $ M.map showT ms
          toStringv (XMapInt ms) = XMapString $ M.map showT ms
          toStringv (XMapString ms) = head xs
          toStringv (XMapBool ms) = XMapString $ M.map showT ms
          toStringv (XMapDate ms) = XMapString $ M.map ((T.dropEnd 4) . showT) ms

trim :: OperationMode -> [XMap] -> XMapErr
trim om = XFunction.apply T.strip

trimLeft :: OperationMode -> [XMap] -> XMapErr
trimLeft om = XFunction.apply T.stripStart

trimRight :: OperationMode -> [XMap] -> XMapErr
trimRight om = XFunction.apply T.stripEnd

ifThen :: OperationMode -> [XMap] -> XMapErr
ifThen om xs = do
     checkMapsNumber xs 2
     mb <- extractMapBool (head xs) "if"
     ms <- toMapList (tail xs)
     Right $ ifList mb ms
     where
          ifThenV :: Ord k => M.Map k Bool -> M.Map k a -> M.Map k a
          ifThenV mb mv = M.restrictKeys mv (M.keysSet $ M.filterWithKey (\k b -> b) mb)
          ifList mb (XMapDoubleList ms) = XMapDouble $ ifThenV mb (head ms)
          ifList mb (XMapIntList ms) = XMapInt $  ifThenV mb (head ms)
          ifList mb (XMapStringList ms) = XMapString $  ifThenV mb (head ms)
          ifList mb (XMapBoolList ms) = XMapBool $  ifThenV mb (head ms)
          ifList mb (XMapDateList ms) =XMapDate $  ifThenV mb (head ms)

ifThenElse :: OperationMode -> [XMap] -> XMapErr
ifThenElse om xs = do
     checkMapsNumber xs 3
     mb <- extractMapBool (head xs) "if"
     ms <- toMapList (tail xs)
     Right $ ifList mb ms
     where
          ifThenElseV :: Ord k => M.Map k Bool -> M.Map k a -> M.Map k a -> M.Map k a
          ifThenElseV mb m1 m2 = M.mapMaybeWithKey (\k b -> if b then (M.lookup k m1) else (M.lookup k m2)) mb
          second l = (head $ tail l)
          ifList mb (XMapDoubleList ms) = XMapDouble $ ifThenElseV mb (head ms) (second ms)
          ifList mb (XMapIntList ms) = XMapInt $  ifThenElseV mb (head ms) (second ms)
          ifList mb (XMapStringList ms) = XMapString $  ifThenElseV mb (head ms) (second ms)
          ifList mb (XMapBoolList ms) = XMapBool $  ifThenElseV mb (head ms) (second ms)
          ifList mb (XMapDateList ms) =XMapDate $  ifThenElseV mb (head ms) (second ms)

operationRepository :: OperationName -> OperationFun
operationRepository op = case op of
    Add -> add
    Subtract -> fsubtract
    Times -> times
    Divide -> divide
    Negate -> fnegate
    Abs -> fabs
    Sin -> fsin
    Cos -> fcos
    Tan -> ftan
    Exp -> fexp
    Log -> flog
    GreaterThan -> greaterThan
    LessThan -> lessThan
    GreaterOrEqual -> greaterOrEqual
    LessOrEqual -> lessOrEqual
    Sum -> fsum
    Product -> fproduct
    And -> fand
    Or -> for
    Not -> fnot
    IfThen -> ifThen
    IfThenElse -> ifThenElse
    Avg -> average
    KeysTo -> keysTo
    Merge -> merge
    Equals -> equals
    Len -> len
    ToDecimal -> toDecimal
    ToString -> toString
    Trim -> trim
    TrimLeft -> trimLeft
    TrimRight -> trimRight

