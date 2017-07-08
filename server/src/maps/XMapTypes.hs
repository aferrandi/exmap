module XMapTypes (Text(Text), XMapName(XMapName), XMap(..), MapValue, XNamedMap(..), XMapKey(XMapKey), XMapErr, XMapByName) where


import qualified Data.Map.Strict as Map

-- always a string. Do not complicate our life too much. You need integers? Convert them to strings
newtype XMapKey = XMapKey String deriving (Show, Eq, Ord)
-- can be a path
newtype XMapName = XMapName [String] deriving (Show, Eq, Ord)

newtype Text = Text String deriving (Show, Eq)

type MapValue a = Map.Map XMapKey a
data XMap = XMapDouble (MapValue Double) |
            XMapInt (MapValue Int) |
            XMapString (MapValue Text) |
            XMapBool (MapValue Bool)
            deriving (Show, Eq)

type XMapErr = Either String XMap

data XNamedMap = XNamedMap {
    name :: XMapName,
    xmap :: XMap
} deriving Show

type XMapByName = Map.Map XMapName XMap



