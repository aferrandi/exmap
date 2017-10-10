module MapsExtraction exposing (..)

import Dict as Dict
import Regex as Regex exposing (..)

import XMapTypes exposing (..)

mapValues : XMap -> Dict.Dict String String
mapValues m =
    let keyValueToString _ v =  toString v
    in case m of
     XMapInt (MapValue mapInt) -> Dict.map keyValueToString mapInt
     XMapString (MapValue mapString) -> Dict.map keyValueToString mapString
     XMapBool (MapValue mapBool) -> Dict.map keyValueToString mapBool
     XMapDouble (MapValue mapDouble) -> Dict.map keyValueToString mapDouble


mapKeys : XMap -> List XMapKey
mapKeys m = case m of
               XMapInt (MapValue mapInt) -> Dict.keys  mapInt
               XMapString (MapValue mapString) -> Dict.keys  mapString
               XMapBool (MapValue mapBool) -> Dict.keys  mapBool
               XMapDouble (MapValue mapDouble) -> Dict.keys  mapDouble

xmapNameToString : XMapName -> String
xmapNameToString mn = String.join "/" mn

xmapNameFromString : String -> Result String XMapName
xmapNameFromString t = if Regex.contains (Regex.regex "^\\w[\\w\\d]*(/\\w[\\w\\d]*)*") t
                       then Ok (String.split "/" t |> List.map String.trim )
                       else Err "The text must start with a letter and contain only letters, numbers and slashes"