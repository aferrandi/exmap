module DecodeProject exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import String exposing (split)
import Dict exposing (..)

import XMapTypes exposing (..)
import Project exposing (..)
import Views exposing (..)


xmapNameDecoder : Decoder XMapName
xmapNameDecoder = string |> andThen (\s -> succeed (split s "/"))

decodeType : (String -> Decoder a) -> Decoder a
decodeType decodeFromType = field "type" string |> andThen decodeFromType


buildMapContent : List (String, a) -> MapValue a
buildMapContent l = MapValue (fromList l)

xmapDecoder : Decoder XMap
xmapDecoder =
    let decodeFromType d = case d of
            "double" -> field "values" (keyValuePairs float) |> andThen (\s -> succeed (XMapDouble (buildMapContent s)))
            "int" -> (field "values" (keyValuePairs int))|> andThen (\s -> succeed (XMapInt (buildMapContent s)))
            "string" -> (field "values" (keyValuePairs string))|> andThen (\s -> succeed (XMapString (buildMapContent s)))
            "bool" -> (field "values" (keyValuePairs bool))|> andThen (\s -> succeed (XMapBool (buildMapContent s)))
            otherwise -> fail ("map type " ++ d ++ " not recognized")
    in decodeType decodeFromType

xNamedMapDecoder : Decoder XNamedMap
xNamedMapDecoder = decode XNamedMap
                   |> required "mapName" xmapNameDecoder
                   |> required "xmap" xmapDecoder


stringToEnum : Dict String a -> String -> Decoder a
stringToEnum m s = case get s m of
                        Just e -> succeed e
                        Nothing -> fail <| "Unknown : " ++ s

operationNameDecoder : Decoder OperationName
operationNameDecoder =
    let m = fromList [("Add",Add), ("Subtract",Subtract)]
    in string |> andThen (stringToEnum m)


applicationNameDecoder : Decoder ApplicationName
applicationNameDecoder =
    let  m = fromList [("Negate",Negate)]
    in string |> andThen (stringToEnum m)

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

viewItemDecoder : Decoder ViewItem
viewItemDecoder =
    let decodeFromType t = case t of
                            "map" -> decode MapItem
                                               |> required "mapName" xmapNameDecoder
                            "label" -> decode LabelItem
                                               |> required "label" string
                            otherwise -> fail ("view item type " ++ t ++ " not recognized")
    in decodeType decodeFromType


viewRowDecoder : Decoder ViewRow
viewRowDecoder = field "items" (list viewItemDecoder) |> andThen (\s -> succeed (ViewRow s))

-- vertical on the screen

viewDecoder : Decoder View
viewDecoder = decode View
                   |> required "viewName" string
                   |> required "rows" (list viewRowDecoder)

allProjectsDecoder : Decoder AllProjects
allProjectsDecoder = list string
