module DecodeView exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import String exposing (split)
import Dict exposing (Dict, get, fromList)

import DecodeXMap exposing (..)
import Views exposing (..)


viewItemDecoder : Decoder ViewItem
viewItemDecoder =
    let decodeFromType t = case t of
                            "map" -> decode MapItem
                                               |> required "mapName" xmapNameDecoder
                            "label" -> decode LabelItem
                                               |> required "label" string
                            _ -> fail ("view item type " ++ t ++ " not recognized")
    in decodeType decodeFromType


viewRowDecoder : Decoder ViewRow
viewRowDecoder = field "items" (list viewItemDecoder) |> andThen (\s -> succeed (ViewRow s))

-- vertical on the screen

viewDecoder : Decoder View
viewDecoder = decode View
                   |> required "viewName" string
                   |> required "rows" (list viewRowDecoder)

