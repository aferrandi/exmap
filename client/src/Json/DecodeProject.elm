module Json.DecodeProject exposing (..)

import Json.DecodeXMap exposing (..)
import Json.Decode exposing (..)
import Types.Project exposing (..)


decodeHttpSource : Decoder HttpSourceType
decodeHttpSource =
    map HttpSourceType
        (field "url" string)


decodeOdbcSource : Decoder OdbcSourceType
decodeOdbcSource =
    map2 OdbcSourceType
        (field "connectionString" string)
        (field "sqlQuery" string)


sourceTypeDecoder : Decoder SourceType
sourceTypeDecoder =
    let
        decodeFromType t =
            case t of
                "fileSource" -> succeed FileSource
                "odbcSource" -> decodeOdbcSource |> andThen (\s -> succeed (OdbcSource s))
                "httpSource" -> decodeHttpSource |> andThen (\s -> succeed (HttpSource s))
                otherwise -> fail ("source type " ++ t ++ " not recognized")
    in
        decodeType decodeFromType


sourceDecoder : Decoder Source
sourceDecoder =
    map2 Source
        (field "sourceType" sourceTypeDecoder)
        (field "sourceOfMaps" (list xmapDefinitionDecoder))


projectDecoder : Decoder Project
projectDecoder =
    map4 Project
        (field "projectName" string)
        (field "calculations" (list string))
        (field "views" (list string))
        (field "sources" (list sourceDecoder))


allProjectsDecoder : Decoder AllProjects
allProjectsDecoder =
    list string
