{-# LANGUAGE OverloadedStrings   #-}

module XValues(XValue, defaultValue, extractMap, extractMapFirst, extractMapSecond, buildMap, UnaryXMapFun, BinaryXMapFun) where

import XMapTypes
import qualified Data.Text as T

type UnaryXMapFun a r = a -> r
type BinaryXMapFun a b r = a -> b -> r
type AggregateXMapFun a r = a -> r


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
    extractMapFirst _ _ = Left $ Error "The map must be of type double"
    extractMapSecond (XMapInt m) _ = Right m
    extractMapSecond _ _ = Left $ Error "The map must be of type double"
    buildMap = XMapInt
    defaultValue = 0

instance XValue T.Text where
    extractMap (XMapString m) _ = Right m
    extractMap _ _ = Left $ Error "The map must be of type string"
    extractMapFirst (XMapString m) _ = Right m
    extractMapFirst _ _ = Left $ Error "The map must be of type double"
    extractMapSecond (XMapString m) _ = Right m
    extractMapSecond _ _ = Left $ Error "The map must be of type double"
    buildMap = XMapString
    defaultValue = ""

instance XValue Bool where
    extractMap (XMapBool m) _ = Right m
    extractMap _ _ = Left $ Error "The map must be of type bool"
    extractMapFirst (XMapBool m) _ = Right m
    extractMapFirst _ _ = Left $ Error "The map must be of type double"
    extractMapSecond (XMapBool m) _ = Right m
    extractMapSecond _ _ = Left $ Error "The map must be of type double"
    buildMap = XMapBool
    defaultValue = False


