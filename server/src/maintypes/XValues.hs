{-# LANGUAGE OverloadedStrings   #-}

module XValues(XValue, defaultValue, extractMapByFun, extractMapFirstByFun, extractMapSecondByFun, mapMapKeys, toMapList, buildMap, UnaryXMapFun, BinaryXMapFun, size) where

import XMapTypes
import qualified Data.Text as T
import qualified Data.Map.Strict as M

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

size :: XMap -> Int
size (XMapDouble m) = M.size m
size (XMapInt m) = M.size m
size (XMapString m) = M.size m
size (XMapBool m) = M.size m


mapMapKeys :: (XMapKey -> XMapKey) -> XMap -> XMap
mapMapKeys f (XMapDouble m) = XMapDouble $  M.mapKeys f m
mapMapKeys f (XMapInt m) = XMapInt $ M.mapKeys f m
mapMapKeys f (XMapString m) = XMapString $ M.mapKeys f m
mapMapKeys f (XMapBool m) = XMapBool $ M.mapKeys f m

mapToMapList :: XMap -> XMapList
mapToMapList (XMapDouble x) = XMapDoubleList [x]
mapToMapList (XMapInt x) = XMapIntList [x]
mapToMapList (XMapString x) = XMapStringList [x]
mapToMapList (XMapBool x) = XMapBoolList [x]


composeMapList :: XMapList -> XMap -> Either Error XMapList
composeMapList (XMapDoubleList xs) (XMapDouble x) = Right (XMapDoubleList (x:xs))
composeMapList (XMapIntList xs) (XMapInt x) = Right (XMapIntList (x:xs))
composeMapList (XMapStringList xs) (XMapString x) = Right (XMapStringList (x:xs))
composeMapList (XMapBoolList xs) (XMapBool x) = Right (XMapBoolList (x:xs))
composeMapList _ _ = Left $ Error "List of different types of maps"


toMapList:: [XMap] -> Either Error XMapList
toMapList (x:y:xs) = do
                    ms <- toMapList (y:xs)
                    composeMapList ms x
toMapList (x:_) = Right (mapToMapList x)
