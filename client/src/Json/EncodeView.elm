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

encodeViewRowHeaderType : ViewRowHedersType -> Value
encodeViewRowHeaderType idt =
    case idt of
        RowHasHeader -> string "RowHasHeader"
        RowNoHeader-> string "RowNoHeader"


encodeViewRow : ViewRow -> Value
encodeViewRow r =
        object [
            ( "items", list encodeViewItem r.items ),
            ( "headerType", encodeViewRowHeaderType r.headerType)
        ]

encodeView : View -> Value
encodeView v =
    object
        [ ( "viewName", string v.viewName )
        , ( "rows", list encodeViewRow v.rows)
        ]
