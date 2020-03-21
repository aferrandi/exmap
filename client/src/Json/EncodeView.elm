module Json.EncodeView exposing (..)

import Json.EncodeXMap exposing (..)
import Json.Encode exposing (..)
import Types.Views exposing (..)

encodeViewItem : ViewItem -> Value
encodeViewItem i =
    case i of
        MapItem mn ->
            object
                [ ( "type", string "map" )
                , ( "mapName", encodeXmapName mn )
                ]
        LabelItem l ->
            object
                [ ( "type", string "label" )
                , ( "label", string l )
                ]

encodeViewRowidsType : ViewRowIdsType -> Value
encodeViewRowidsType idt =
    case idt of
        RowHasIds -> string "RowHasIds"
        RowNoIds-> string "RowNoIds"


encodeViewRow : ViewRow -> Value
encodeViewRow r =
        object [
            ( "items", list encodeViewItem r.items ),
            ( "idsType", encodeViewRowidsType r.idsType)
        ]

encodeView : View -> Value
encodeView v =
    object
        [ ( "viewName", string v.viewName )
        , ( "rows", list encodeViewRow v.rows)
        ]
