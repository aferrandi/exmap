module XMapJsonTest (
                        toParseJSON_doublemap_same,
                        toParseJSON_stringmap_same) where
import Data.Aeson
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Debug.Trace


import XMapTypes
import TestTypes
import XMapJson



toParseJSON_doublemap_same = TestCase (assertEqual "map -> json -> map" (Just original) (decode . encodeTrace $ original))
    where original =  XNamedMap {
        xmapName = mapName ["map"],
        xmap = makeDoubleXMap [("k1",13),("k2",24)]
    }


toParseJSON_stringmap_same = TestCase (assertEqual "map -> json -> map" (Just original) (decode . encodeTrace $ original))
    where original =  XNamedMap {
        xmapName = mapName ["map"],
        xmap = makeStringXMap [("k1","a"),("k2","b")]
    }

encodeTrace :: ToJSON a => a -> B.ByteString
encodeTrace = traceJson . encode
    where traceJson json = trace ("json" ++ show json) json
