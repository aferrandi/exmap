module DecodeView exposing (..)

import DecodeXMap exposing (..)
import Dict exposing (Dict, fromList, get)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import String exposing (split)
import Views exposing (..)


viewItemDecoder : Decoder ViewItem
viewItemDecoder =
    let
        decodeFromType t =
            case t of
                "map" -> map MapItem (field "mapName" xmapNameDecoder)
                "label" -> map LabelItem (field "label" string)
                _ ->   fail ("view item type " ++ t ++ " not recognized")
    in
        decodeType decodeFromType


viewRowDecoder : Decoder ViewRow
viewRowDecoder =
    field "items" (list viewItemDecoder) |> andThen (\s -> succeed (ViewRow s))



-- vertical on the screen


viewDecoder : Decoder View
viewDecoder =
    map2 View
        (field "viewName" string)
        (field "rows" (list viewRowDecoder))
