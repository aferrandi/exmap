module TestTypes where
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import XMapTypes
import XValues

makeMap :: XValue a => [(String, a)] -> MapValue a
makeMap l = M.fromList (map (\(k, n) -> (XMapKey (T.pack k), n)) l)

makeXMap :: XValue a => [(String, a)] -> XMap
makeXMap l = buildMap $ makeMap l

mapName :: [String] -> XMapName
mapName ss = XMapName $ map T.pack ss

mapValuesToText :: [(String, String)] -> [(String, T.Text)]
mapValuesToText = map (\(k, n) -> (k, T.pack n))

makeStringXMap :: [(String, String)] -> XMap
makeStringXMap = makeXMap . mapValuesToText

makeDoubleXMap :: [(String, Double)] -> XMap
makeDoubleXMap = makeXMap