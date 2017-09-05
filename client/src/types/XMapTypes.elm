module XMapTypes exposing (..)

import Dict exposing (Dict)

type alias XMapKey = String
-- can be a path
type XMapName = XMapName (List String)

type MapValue a = MapValue (Dict XMapKey a)

type XMap = XMapDouble (MapValue Float)
            | XMapInt (MapValue Int)
            | XMapString (MapValue String)
            | XMapBool (MapValue Bool)

type alias XNamedMap = { xmapName : XMapName, xmap : XMap }
