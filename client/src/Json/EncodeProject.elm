module Json.EncodeProject exposing (..)

import Json.EncodeXMap exposing (..)
import Json.Encode exposing (..)
import Types.Project exposing (..)


encodeInternalSource : Value
encodeInternalSource =
    object
        [ ( "type", string "internalSource" ) ]


encodeHttpSource : HttpSourceType -> Value
encodeHttpSource st =
    object
        [ ( "type", string "httpSource" )
        , ( "url", string st.url )
        ]


encodeOdbcSource : OdbcSourceType -> Value
encodeOdbcSource st =
    object
        [ ( "type", string "odbcSource" )
        , ( "connectionString", string st.connectionString )
        , ( "sqlQuery", string st.sqlQuery )
        ]


encodeSourceType : SourceType -> Value
encodeSourceType st =
    case st of
        FileSource -> encodeInternalSource
        HttpSource v -> encodeHttpSource v
        OdbcSource v -> encodeOdbcSource v


encodeSource : Source -> Value
encodeSource s =
    object
        [ ( "sourceType", encodeSourceType s.sourceType )
        , ( "sourceOfMaps", list encodeXMapDefinition s.sourceOfMaps)
        ]


encodeProject : Project -> Value
encodeProject p =
    object
        [ ( "projectName", string p.projectName )
        , ( "calculations", list string p.calculations)
        , ( "views", list string p.viewNames)
        , ( "sources", list encodeSource p.sources)
        ]
