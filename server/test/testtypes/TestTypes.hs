module TestTypes where
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import XMapTypes

makeXMap :: [(String, Double)] -> XMap
makeXMap l = XMapDouble $ M.fromList (map (\(k, n) -> (XMapKey (T.pack k), n)) l)

mapName :: [String] -> XMapName
mapName ss = XMapName $ map T.pack ss