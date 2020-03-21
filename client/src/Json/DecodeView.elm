module Json.DecodeView exposing (..)

import Dict exposing (fromList)
import Json.DecodeXMap exposing (..)
import Json.Decode exposing (..)
import Json.EnumToString exposing (stringToEnum)
import Types.Views exposing (..)

viewRowidsTypeDecoder : Decoder ViewRowIdsType
viewRowidsTypeDecoder =
    let
        m = fromList [ ( "RowHasIds", RowHasIds ), ( "RowNoIds", RowNoIds ) ]
    in
        string |> andThen (stringToEnum m)

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
    map2 ViewRow
        (field "items" (list viewItemDecoder))
        (field "idsType" viewRowidsTypeDecoder)

-- vertical on the screen
viewDecoder : Decoder View
viewDecoder =
    map2 View
        (field "viewName" string)
        (field "rows" (list viewRowDecoder))
