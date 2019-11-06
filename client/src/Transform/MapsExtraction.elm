module Transform.MapsExtraction exposing (..)

import Dict as Dict
import Iso8601
import Regex as Regex exposing (..)
import Types.XMapTypes exposing (..)

fromBool : Bool -> String
fromBool b = case b of
    True -> "True"
    False -> "False"

mapValues : XMap -> Dict.Dict String String
mapValues m =
    case m of
        XMapInt (MapValue mapInt) ->
            Dict.map (\_ v -> String.fromInt v) mapInt
        XMapString (MapValue mapString) ->
            Dict.map (\_ v -> v) mapString
        XMapBool (MapValue mapBool) ->
            Dict.map (\_ v -> fromBool v) mapBool
        XMapDouble (MapValue mapDouble) ->
            Dict.map (\k v -> String.fromFloat v) mapDouble
        XMapDate (MapValue mapDate) ->
            Dict.map (\k v -> Iso8601.fromTime v) mapDate

mapKeys : XMap -> List XMapKey
mapKeys m =
    case m of
        XMapInt (MapValue mapInt) -> Dict.keys mapInt
        XMapString (MapValue mapString) -> Dict.keys mapString
        XMapBool (MapValue mapBool) -> Dict.keys mapBool
        XMapDouble (MapValue mapDouble) -> Dict.keys mapDouble
        XMapDate (MapValue mapDate) -> Dict.keys mapDate

xmapNameToString : XMapName -> String
xmapNameToString mn =
    String.join "/" mn

xmapNameFromString : String -> Result String XMapName
xmapNameFromString t =
    if Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "^[a-zA-Z]\\w*(\\/[a-zA-Z]\\w*)*$") t then
        Ok (String.split "/" t |> List.map String.trim)
    else
        Err "The text must start with a letter and contain only letters, numbers and slashes"
