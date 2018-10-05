{-# LANGUAGE OverloadedStrings   #-}

module XValues(XValue, defaultValue, extractMap, extractMapFirst, extractMapSecond, toMapList, buildMap, UnaryXMapFun, BinaryXMapFun, size) where

import XMapTypes
import qualified Data.Text as T
import qualified Data.Map.Strict as M

type UnaryXMapFun a r = a -> r
type BinaryXMapFun a b r = a -> b -> r
--type AggregateXMapFun a r = a -> r


class XValue a where
    extractMap :: XMap -> UnaryXMapFun a r -> Either Error (MapValue a)
    extractMapFirst :: XMap -> BinaryXMapFun a s r -> Either Error (MapValue a)
    extractMapSecond :: XMap -> BinaryXMapFun s a r -> Either Error (MapValue a)
    buildMap :: MapValue a -> XMap
    defaultValue :: a


instance XValue Double where
    extractMap (XMapDouble m) _ = Right m
    extractMap _ _ = Left $ Error "The map must be of type double"
    extractMapFirst (XMapDouble m) _ = Right m
    extractMapFirst _ _ = Left $ Error "The map must be of type double"
    extractMapSecond (XMapDouble m) _ = Right m
    extractMapSecond _ _ = Left $ Error "The map must be of type double"
    buildMap  = XMapDouble
    defaultValue = 0.0

instance XValue Int where
    extractMap (XMapInt m) _ = Right m
    extractMap _ _ = Left $ Error "The map must be of type int"
    extractMapFirst (XMapInt m) _ = Right m
    extractMapFirst _ _ = Left $ Error "The map must be of type int"
    extractMapSecond (XMapInt m) _ = Right m
    extractMapSecond _ _ = Left $ Error "The map must be of type int"
    buildMap  = XMapInt
    defaultValue = 0

instance XValue T.Text where
    extractMap (XMapString m) _ = Right m
    extractMap _ _ = Left $ Error "The map must be of type string"
    extractMapFirst (XMapString m) _ = Right m
    extractMapFirst _ _ = Left $ Error "The map must be of type string"
    extractMapSecond (XMapString m) _ = Right m
    extractMapSecond _ _ = Left $ Error "The map must be of type string"
    buildMap  = XMapString
    defaultValue = ""

instance XValue Bool where
    extractMap (XMapBool m) _ = Right m
    extractMap _ _ = Left $ Error "The map must be of type bool"
    extractMapFirst (XMapBool m) _ = Right m
    extractMapFirst _ _ = Left $ Error "The map must be of type bool"
    extractMapSecond (XMapBool m) _ = Right m
    extractMapSecond _ _ = Left $ Error "The map must be of type bool"
    buildMap = XMapBool
    defaultValue = False

size :: XMap -> Int
size (XMapDouble m) = M.size m
size (XMapInt m) = M.size m
size (XMapString m) = M.size m
size (XMapBool m) = M.size m

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
