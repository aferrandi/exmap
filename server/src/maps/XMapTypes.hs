module XMapTypes (XMapName(XMapName), XMap(..), MapValue, XNamedMap(..), XMapKey(XMapKey), XMapErr, XMapByName) where


import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- always a string. Do not complicate our life too much. You need integers? Convert them to strings
newtype XMapKey = XMapKey T.Text deriving (Show, Eq, Ord)
-- can be a path
newtype XMapName = XMapName [T.Text] deriving (Show, Eq, Ord)

type MapValue a = M.Map XMapKey a
data XMap = XMapDouble (MapValue Double) |
            XMapInt (MapValue Int) |
            XMapString (MapValue T.Text) |
            XMapBool (MapValue Bool)
            deriving (Show, Eq)

type XMapErr = Either T.Text XMap

data XNamedMap = XNamedMap {
    name :: XMapName,
    xmap :: XMap
} deriving Show

type XMapByName = M.Map XMapName XMap





