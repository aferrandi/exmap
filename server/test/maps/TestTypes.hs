module TestTypes where
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import XMapTypes

singletonXMap :: String -> Double -> XMap
singletonXMap k n = XMapDouble $ M.singleton (XMapKey (T.pack k)) n

mapName :: [String] -> XMapName
mapName ss = XMapName $ map T.pack ss