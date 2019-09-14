module DecodeXMap exposing (..)

import Dict exposing (Dict, fromList, get)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Project exposing (..)
import String exposing (split)
import Views exposing (..)
import XMapTypes exposing (..)


xmapNameDecoder : Decoder XMapName
xmapNameDecoder =
    string |> andThen (\s -> succeed (split "/" s))


decodeType : (String -> Decoder a) -> Decoder a
decodeType decodeFromType =
    field "type" string |> andThen decodeFromType


buildMapContent : List ( String, a ) -> MapValue a
buildMapContent l =
    MapValue (fromList l)


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

                _ ->
                    fail ("map type " ++ d ++ " not recognized")
    in
    decodeType decodeFromType


xNamedMapDecoder : Decoder XNamedMap
xNamedMapDecoder =
    map2 XNamedMap
        (field "mapName" xmapNameDecoder)
        (field "xmap" xmapDecoder)


xmapTypeDecoder : Decoder XMapType
xmapTypeDecoder =
    let
        decodeFromType t =
            case t of
                "double" ->
                    succeed TypeDouble

                "int" ->
                    succeed TypeInt

                "string" ->
                    succeed TypeString

                "bool" ->
                    succeed TypeBool

                otherwise ->
                    fail ("xmap type " ++ t ++ " not recognized")
    in
    string |> andThen decodeFromType
