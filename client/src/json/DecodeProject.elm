module DecodeProject exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import String exposing (split)
import Dict exposing (Dict, get, fromList)

import DecodeXMap exposing (..)
import Project exposing (..)
import Views exposing (..)
import EnumToString exposing (stringToEnum)


decodeHttpSource : Decoder HttpSourceType
decodeHttpSource = decode HttpSourceType
                      |> required "url" string

decodeOdbcSource : Decoder OdbcSourceType
decodeOdbcSource = decode OdbcSourceType
                      |> required "connectionString" string
                      |> required "sqlQuery" string

sourceTypeDecoder : Decoder SourceType
sourceTypeDecoder =
    let decodeFromType t = case t of
                            "fileSource" -> succeed FileSource
                            "odbcSource" -> decodeOdbcSource |> andThen (\s -> succeed (OdbcSource s))
                            "httpSource" -> decodeHttpSource |> andThen (\s -> succeed (HttpSource s))
                            otherwise -> fail ("source type " ++ t ++ " not recognized")
    in decodeType decodeFromType

sourceDecoder : Decoder Source
sourceDecoder = decode Source
                   |> required "sourceType" sourceTypeDecoder
                   |> required "sourceOfMaps" (list xmapNameDecoder)


projectDecoder : Decoder Project
projectDecoder = decode Project
                   |> required "projectName" string
                   |> required "calculations" (list string)
                   |> required "views" (list string)
                   |> required "sources" (list sourceDecoder)


allProjectsDecoder : Decoder AllProjects
allProjectsDecoder = list string