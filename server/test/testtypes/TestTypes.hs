module TestTypes where
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import XMapTypes

makeDoubleXMap :: [(String, Double)] -> XMap
makeDoubleXMap l = XMapDouble $ M.fromList (map (\(k, n) -> (XMapKey (T.pack k), n)) l)

makeStringXMap :: [(String, String)] -> XMap
makeStringXMap l = XMapString $ M.fromList (map (\(k, n) -> (XMapKey (T.pack k), (T.pack n))) l)


mapName :: [String] -> XMapName
mapName ss = XMapName $ map T.pack ss

