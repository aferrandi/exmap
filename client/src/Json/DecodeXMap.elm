module Json.DecodeXMap exposing (..)

import Dict exposing (Dict, fromList)
import Iso8601
import Json.Decode exposing (..)
import String exposing (split)
import Types.XMapTypes exposing (..)


xmapNameDecoder : Decoder XMapName
xmapNameDecoder =
    string |> andThen (\s -> succeed (split "/" s))


decodeType : (String -> Decoder a) -> Decoder a
decodeType decodeFromType =
    field "type" string |> andThen decodeFromType


buildMapContent : List ( String, a ) -> MapValue a
buildMapContent l = MapValue (fromList l)


xmapDecoder : Decoder XMap
xmapDecoder =
    let
        decodeFromType d =
            case d of
                "double" ->
                    field "values" (keyValuePairs float) |> andThen (\s -> succeed (XMapDouble (buildMapContent s)))
                "int" ->
                    field "values" (keyValuePairs int) |> andThen (\s -> succeed (XMapInt (buildMapContent s)))
                "string" ->
                    field "values" (keyValuePairs string) |> andThen (\s -> succeed (XMapString (buildMapContent s)))
                "bool" ->
                    field "values" (keyValuePairs bool) |> andThen (\s -> succeed (XMapBool (buildMapContent s)))
                "date" ->
                    field "values" (keyValuePairs Iso8601.decoder) |> andThen (\s -> succeed (XMapDate (buildMapContent s)))
                _ ->
                    fail ("map type " ++ d ++ " not recognized")
    in
        decodeType decodeFromType

xmapTypeDecoder : Decoder XMapType
xmapTypeDecoder =
    let
        decodeFromType t =
            case t of
                "double" -> succeed TypeDouble
                "int" -> succeed TypeInt
                "string" -> succeed TypeString
                "bool" -> succeed TypeBool
                "date" -> succeed TypeDate
                otherwise -> fail ("xmap type " ++ t ++ " not recognized")
    in
        string |> andThen decodeFromType

xmapDefinitionDecoder : Decoder XMapDefinition
xmapDefinitionDecoder =
    map2 XMapDefinition
        (field "mapName" xmapNameDecoder)
        (field "mapType" xmapTypeDecoder)

xNamedMapDecoder : Decoder XNamedMap
xNamedMapDecoder =
    map2 XNamedMap
        (field "mapDef" xmapDefinitionDecoder)
        (field "xmap" xmapDecoder)

