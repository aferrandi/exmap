module Types.XMapTypes exposing (..)

import Dict exposing (Dict)


type alias XMapKey = String

-- can be a path
type alias XMapName = List String

type MapValue a
    = MapValue (Dict XMapKey a)

type XMap
    = XMapDouble (MapValue Float)
    | XMapInt (MapValue Int)
    | XMapString (MapValue String)
    | XMapBool (MapValue Bool)

type XMapType
    = TypeDouble
    | TypeInt
    | TypeString
    | TypeBool

type alias XNamedMap =
    { xmapName : XMapName
    , xmap : XMap
    }

mapType : XMap -> XMapType
mapType m =
    case m of
        XMapDouble _ -> TypeDouble
        XMapInt _ -> TypeInt
        XMapString _ -> TypeString
        XMapBool _ -> TypeBool
