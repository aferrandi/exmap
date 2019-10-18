module XFunction (XMapErr, OperationMode(..), operate, apply) where

import XMapTypes
import XValues
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe

data OperationMode = Union | Intersection
    deriving (Show, Eq, Read)

unionWith3 :: (XValue a, XValue b) => Ord k => (a -> b -> c) -> M.Map k a -> M.Map k b -> M.Map k c
unionWith3 f a b = S.foldr addKey M.empty keys
    where keys = S.union (M.keysSet a) (M.keysSet b)
          getOrDefault k m = fromMaybe defaultValue (M.lookup k m)
          newValue k = f (getOrDefault k a) (getOrDefault k b)
          addKey k m = M.insert k (newValue k) m


operate :: (XValue a, XValue b, XValue r) => OperationMode -> BinaryXMapFun a b r -> [XMap] -> XMapErr
operate om f (a:b:_) = do
    ea  <- extractMapFirstByFun a f
    eb  <- extractMapSecondByFun b f
    return $ buildMap (buildWithMode ea eb)
    where buildWithMode ea eb = case om of Intersection -> M.intersectionWith f ea eb
                                           Union -> unionWith3 f ea eb

apply :: (XValue a, XValue r) => UnaryXMapFun a r-> [XMap] -> XMapErr
apply f (a:_) = do
    em  <- extractMapByFun a f
    return (buildMap $ M.map f em)
