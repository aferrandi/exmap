{-# LANGUAGE OverloadedStrings   #-}

module XValues(XValue, defaultValue, extractMapByFun,
extractMapFirstByFun, extractMapSecondByFun, mapMapKeys, restrictMapKeys,
toMapList, buildMap, UnaryXMapFun, BinaryXMapFun, size,
extractMapDouble, extractMapInt, extractMapString, extractMapBool, checkMapsNumber) where

import XMapTypes
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Time.Clock as DT
import qualified Data.Time.Calendar as C

import Errors (mkError)

type UnaryXMapFun a r = a -> r
type BinaryXMapFun a b r = a -> b -> r
--type AggregateXMapFun a r = a -> r

mapBoth :: Ord k => ((k, a) -> (k, a)) -> M.Map k a -> M.Map k a
mapBoth f m = M.fromList $ map f (M.toList m)

-- the whole class is made to extract the matrices from the XMap depending by the types of the function
class XValue a where
    extractMapByFun :: XMap -> UnaryXMapFun a r -> Either Error (MapValue a)
    extractMapFirstByFun :: XMap -> BinaryXMapFun a s r -> Either Error (MapValue a)
    extractMapSecondByFun :: XMap -> BinaryXMapFun s a r -> Either Error (MapValue a)
    buildMap :: MapValue a -> XMap
    defaultValue :: a

instance XValue Double where
    extractMapByFun (XMapDouble m) _ = Right m
    extractMapByFun _ _ = Left $ Error "The map must be of type double"
    extractMapFirstByFun (XMapDouble m) _ = Right m
    extractMapFirstByFun _ _ = Left $ Error "The map must be of type double"
    extractMapSecondByFun (XMapDouble m) _ = Right m
    extractMapSecondByFun _ _ = Left $ Error "The map must be of type double"
    buildMap  = XMapDouble
    defaultValue = 0.0

instance XValue Int where
    extractMapByFun (XMapInt m) _ = Right m
    extractMapByFun _ _ = Left $ Error "The map must be of type int"
    extractMapFirstByFun (XMapInt m) _ = Right m
    extractMapFirstByFun _ _ = Left $ Error "The map must be of type int"
    extractMapSecondByFun (XMapInt m) _ = Right m
    extractMapSecondByFun _ _ = Left $ Error "The map must be of type int"
    buildMap  = XMapInt
    defaultValue = 0

instance XValue T.Text where
    extractMapByFun (XMapString m) _ = Right m
    extractMapByFun _ _ = Left $ Error "The map must be of type string"
    extractMapFirstByFun (XMapString m) _ = Right m
    extractMapFirstByFun _ _ = Left $ Error "The map must be of type string"
    extractMapSecondByFun (XMapString m) _ = Right m
    extractMapSecondByFun _ _ = Left $ Error "The map must be of type string"
    buildMap  = XMapString
    defaultValue = ""

instance XValue Bool where
    extractMapByFun (XMapBool m) _ = Right m
    extractMapByFun _ _ = Left $ Error "The map must be of type bool"
    extractMapFirstByFun (XMapBool m) _ = Right m
    extractMapFirstByFun _ _ = Left $ Error "The map must be of type bool"
    extractMapSecondByFun (XMapBool m) _ = Right m
    extractMapSecondByFun _ _ = Left $ Error "The map must be of type bool"
    buildMap = XMapBool
    defaultValue = False

instance XValue DT.UTCTime where
    extractMapByFun (XMapDate m) _ = Right m
    extractMapByFun _ _ = Left $ Error "The map must be of type date"
    extractMapFirstByFun (XMapDate m) _ = Right m
    extractMapFirstByFun _ _ = Left $ Error "The map must be of type date"
    extractMapSecondByFun (XMapDate m) _ = Right m
    extractMapSecondByFun _ _ = Left $ Error "The map must be of type date"
    buildMap = XMapDate
    defaultValue = DT.UTCTime (C.fromGregorian 0 1 1) (DT.secondsToDiffTime 0)


size :: XMap -> Int
size (XMapDouble m) = M.size m
size (XMapInt m) = M.size m
size (XMapString m) = M.size m
size (XMapBool m) = M.size m
size (XMapDate m) = M.size m


mapMapKeys :: (XMapKey -> XMapKey) -> XMap -> XMap
mapMapKeys f (XMapDouble m) = XMapDouble $  M.mapKeys f m
mapMapKeys f (XMapInt m) = XMapInt $ M.mapKeys f m
mapMapKeys f (XMapString m) = XMapString $ M.mapKeys f m
mapMapKeys f (XMapBool m) = XMapBool $ M.mapKeys f m
mapMapKeys f (XMapDate m) = XMapDate $ M.mapKeys f m


restrictMapKeys :: S.Set XMapKey -> XMap -> XMap
restrictMapKeys s (XMapDouble m) = XMapDouble $ M.restrictKeys m s
restrictMapKeys s (XMapInt m) = XMapInt $ M.restrictKeys m s
restrictMapKeys s (XMapString m) = XMapString $ M.restrictKeys m s
restrictMapKeys s (XMapBool m) = XMapBool $ M.restrictKeys m s
restrictMapKeys s (XMapDate m) = XMapDate $ M.restrictKeys m s

mapToMapList :: XMap -> XMapList
mapToMapList (XMapDouble x) = XMapDoubleList [x]
mapToMapList (XMapInt x) = XMapIntList [x]
mapToMapList (XMapString x) = XMapStringList [x]
mapToMapList (XMapBool x) = XMapBoolList [x]
mapToMapList (XMapDate x) = XMapDateList [x]

composeMapList :: XMapList -> XMap -> Either Error XMapList
composeMapList (XMapDoubleList xs) (XMapDouble x) = Right (XMapDoubleList (x:xs))
composeMapList (XMapIntList xs) (XMapInt x) = Right (XMapIntList (x:xs))
composeMapList (XMapStringList xs) (XMapString x) = Right (XMapStringList (x:xs))
composeMapList (XMapBoolList xs) (XMapBool x) = Right (XMapBoolList (x:xs))
composeMapList (XMapDateList xs) (XMapDate x) = Right (XMapDateList (x:xs))
composeMapList _ _ = Left $ Error "List of different types of maps"


toMapList:: [XMap] -> Either Error XMapList
toMapList (x:y:xs) = do
                    ms <- toMapList (y:xs)
                    composeMapList ms x
toMapList (x:_) = Right (mapToMapList x)


extractMapDouble :: XMap -> T.Text -> Either Error (MapValue Double)
extractMapDouble (XMapDouble x) _ = Right x
extractMapDouble _ nm = Left $ mkError ("The " ++ (show nm) ++ " map must be of type double")

extractMapInt :: XMap -> T.Text -> Either Error (MapValue Int)
extractMapInt (XMapInt x) _ = Right x
extractMapInt _ nm = Left $ mkError ("The " ++ (show nm) ++ " map must be of type int")

extractMapString :: XMap -> T.Text -> Either Error (MapValue T.Text)
extractMapString (XMapString x) _ = Right x
extractMapString _ nm = Left $ mkError ("The " ++ (show nm) ++ " map must be of type text")

extractMapBool :: XMap -> T.Text -> Either Error (MapValue Bool)
extractMapBool (XMapBool x) _ = Right x
extractMapBool _ nm = Left $ mkError ("The " ++ (show nm) ++ " map must be of type bool")

extractMapDate :: XMap -> T.Text -> Either Error (MapValue DT.UTCTime)
extractMapDate (XMapDate x) _ = Right x
extractMapDate _ nm = Left $ mkError ("The " ++ (show nm) ++ " map must be of type date")

checkMapsNumber :: [XMap] -> Int -> Either Error ()
checkMapsNumber xs n = if length xs < n then
                          Left $ mkError ("Too few maps. Should be at least " ++ show n)
                       else
                          Right ()
