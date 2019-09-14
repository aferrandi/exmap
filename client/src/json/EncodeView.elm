module EncodeView exposing (..)

import EncodeXMap exposing (..)
import Json.Encode exposing (..)
import Views exposing (..)


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


encodeViewRow : ViewRow -> Value
encodeViewRow r =
    let
        items (ViewRow is) =
            is
    in
    object
        [ ( "items", list encodeViewItem (items r))
        ]


encodeView : View -> Value
encodeView v =
    object
        [ ( "viewName", string v.viewName )
        , ( "rows", list encodeViewRow v.rows)
        ]
