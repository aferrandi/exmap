module DecodeXMap exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import String exposing (split)
import Dict exposing (Dict, get, fromList)

import XMapTypes exposing (..)
import Project exposing (..)
import Views exposing (..)


xmapNameDecoder : Decoder XMapName
xmapNameDecoder = string |> andThen (\s -> succeed (split "/" s ))

decodeType : (String -> Decoder a) -> Decoder a
decodeType decodeFromType = field "type" string |> andThen decodeFromType


buildMapContent : List (String, a) -> MapValue a
buildMapContent l = MapValue (fromList l)

xmapDecoder : Decoder XMap
xmapDecoder =
    let decodeFromType d = case d of
            "double" -> field "values" (keyValuePairs float) |> andThen (\s -> succeed (XMapDouble (buildMapContent s)))
            "int" -> field "values" (keyValuePairs int)|> andThen (\s -> succeed (XMapInt (buildMapContent s)))
            "string" -> field "values" (keyValuePairs string)|> andThen (\s -> succeed (XMapString (buildMapContent s)))
            "bool" -> field "values" (keyValuePairs bool)|> andThen (\s -> succeed (XMapBool (buildMapContent s)))
            _ -> fail ("map type " ++ d ++ " not recognized")
    in decodeType decodeFromType

xNamedMapDecoder : Decoder XNamedMap
xNamedMapDecoder = decode XNamedMap
                   |> required "mapName" xmapNameDecoder
                   |> required "xmap" xmapDecoder
