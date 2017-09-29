module EncodeProject exposing (..)

import Json.Encode exposing (..)
import String exposing (join)
import Dict exposing (..)

import XMapTypes exposing (..)
import Project exposing (..)
import Views exposing (..)

encodeXmapName : XMapName -> Value
encodeXmapName mn = string (join "/" mn)



encodeMapContent : (a-> Value) -> MapValue a -> Value
encodeMapContent encodeMapValue (MapValue mv)  =
    let encodeMapItem k v = encodeMapValue v
    in Dict.map encodeMapItem mv
        |> toList
        |> object


encodeXMap : XMap -> Value
encodeXMap m = case m of
    (XMapDouble v) -> object
                            [ ("type", string "double")
                            , ("values", encodeMapContent float v)
                            ]
    (XMapInt v) -> object
                            [ ("type", string "int")
                            , ("values", encodeMapContent int v)
                            ]
    (XMapString v) -> object
                            [ ("type", string "string")
                            , ("values", encodeMapContent string v)
                            ]
    (XMapBool v) ->  object
                            [ ("type", string "bool")
                            , ("values", encodeMapContent bool v)
                            ]

encodeXNamedMap : XNamedMap -> Value
encodeXNamedMap nm = object
                        [ ("mapName", encodeXmapName nm.xmapName)
                        , ("values", encodeXMap nm.xmap)
                        ]

encodeInternalSource : Value
encodeInternalSource = object
                        [ ("type", string "internalSource") ]

encodeHttpSource : HttpSourceType -> Value
encodeHttpSource st = object
                        [ ("type", string "httpSource")
                        , ("url", string st.url)
                        ]

encodeOdbcSource : OdbcSourceType -> Value
encodeOdbcSource st = object
                      [ ("type", string "odbcSource")
                      ,  ("connectionString", string st.connectionString)
                      , ("sqlQuery", string st.sqlQuery)
                      ]

encodeSourceType : SourceType -> Value
encodeSourceType st = case st of
    FileSource -> encodeInternalSource
    HttpSource v -> encodeHttpSource v
    OdbcSource v -> encodeOdbcSource v

encodeSource : Source -> Value
encodeSource s = object
                   [ ("sourceType", encodeSourceType s.sourceType)
                   , ("sourceOfMaps", List.map encodeXmapName s.sourceOfMaps |> list)
                    ]

encodeProject : Project -> Value
encodeProject p = object
                 [ ("projectName", string p.projectName)
                 , ("calculations", List.map string p.calculations |> list)
                 , ("views", List.map string p.viewNames |> list)
                 , ("sources", List.map encodeSource p.sources |> list)
                  ]



{-
  object
    [ ("type", "loadProject")
    , ("projectName", lp.projectName)
    ]

-}

