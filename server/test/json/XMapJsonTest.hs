module XMapJsonTest (tests) where

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
    where 
      original =  XNamedMap {
          xmapDef = XMapDefinition { xmapName = makeMapName ["map"], xmapType = TypeText },
          xmap = makeXMap arr
      }
      arr :: [(String, Double)]
      arr = [("k1",13.0),("k2",24.0)]


toParseJSON_stringmap_same = TestCase (assertEqual "map -> json -> map" (Just original) (decode . encodeTrace $ original))
    where original =  XNamedMap {
        xmapDef = XMapDefinition  { xmapName = makeMapName ["map"], xmapType = TypeText },
        xmap = (makeXMap . mapValuesToText) [("k1","a"),("k2","b")]
    }

encodeTrace :: ToJSON a => a -> B.ByteString
encodeTrace = traceJson . encode
    where traceJson json = trace ("json" ++ show json) json

tests = [
            toParseJSON_doublemap_same,
            toParseJSON_stringmap_same
        ]