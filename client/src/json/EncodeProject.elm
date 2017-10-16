module EncodeProject exposing (..)

import Json.Encode exposing (..)

import EncodeXMap exposing (..)
import Project exposing (..)
import Calculation exposing (..)
import Views exposing (..)

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
