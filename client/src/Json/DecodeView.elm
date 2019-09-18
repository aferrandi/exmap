module Json.DecodeView exposing (..)

import Json.DecodeXMap exposing (..)
import Json.Decode exposing (..)
import Types.Views exposing (..)


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
