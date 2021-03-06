module Json.EncodeXMap exposing (..)

import Dict exposing (..)
import Iso8601
import Json.Encode exposing (..)
import String exposing (join)
import Types.XMapTypes exposing (..)


encodeXmapName : XMapName -> Value
encodeXmapName mn =
    string (join "/" mn)


encodeMapContent : (a -> Value) -> MapValue a -> Value
encodeMapContent encodeMapValue (MapValue mv) =
    let
        encodeMapItem _ v = encodeMapValue v
    in
        Dict.map encodeMapItem mv
        |> toList
        |> object


encodeXMap : XMap -> Value
encodeXMap m =
    case m of
        XMapDouble v ->
            object
                [ ( "type", string "double" )
                , ( "values", encodeMapContent float v )
                ]
        XMapInt v ->
            object
                [ ( "type", string "int" )
                , ( "values", encodeMapContent int v )
                ]
        XMapString v ->
            object
                [ ( "type", string "string" )
                , ( "values", encodeMapContent string v )
                ]
        XMapBool v ->
            object
                [ ( "type", string "bool" )
                , ( "values", encodeMapContent bool v )
                ]
        XMapDate v ->
            object
                [ ( "type", string "date" )
                , ( "values", encodeMapContent Iso8601.encode v )
                ]

encodeXMapType : XMapType -> Value
encodeXMapType mt =
    case mt of
        TypeDouble -> string "double"
        TypeInt -> string "int"
        TypeString -> string "string"
        TypeBool -> string "bool"
        TypeDate -> string "date"

encodeXMapDefinition : XMapDefinition -> Value
encodeXMapDefinition md =
    object
        [ ( "mapName", encodeXmapName md.xmapName )
        , ( "mapType", encodeXMapType md.xmapType )
        ]

encodeXNamedMap : XNamedMap -> Value
encodeXNamedMap nm =
    object
        [ ( "mapDef", encodeXMapDefinition nm.xmapDef )
        , ( "xmap", encodeXMap nm.xmap )
        ]
