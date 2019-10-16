module TestTypes where
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import XMapTypes

makeDoubleMap :: [(String, Double)] -> MapValue Double
makeDoubleMap l = M.fromList (map (\(k, n) -> (XMapKey (T.pack k), n)) l)


makeDoubleXMap :: [(String, Double)] -> XMap
makeDoubleXMap l = XMapDouble $ makeDoubleMap l

makeStringXMap :: [(String, String)] -> XMap
makeStringXMap l = XMapString $ M.fromList (map (\(k, n) -> (XMapKey (T.pack k), (T.pack n))) l)


mapName :: [String] -> XMapName
mapName ss = XMapName $ map T.pack ss

