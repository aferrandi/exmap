module DecodeWebEvent exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import WebMessages exposing (..)
import DecodeProject exposing (..)

webEventDecoder : Decoder WebEvent
webEventDecoder =
    let decodeFromType t = case t of
                            "allProjects" -> decode WEAllProjects
                                                |> required "allProjects" allProjectsDecoder
                            "viewChanged" -> decode WEViewChanged
                                                |> required "projectName" string
                                                |> required "viewName" string
                                                |> required "map" xNamedMapDecoder
                            "projectContent" -> decode WEProjectContent
                                                |> required "project" projectDecoder
                            "projectStored" -> decode WEProjectStored
                                                |> required "projectName" string
                            "mapLoaded" -> decode WEMapLoaded
                                                |> required "projectName" string
                                                |> required "map" xNamedMapDecoder
                            "mapStored" -> decode WEMapStored
                                                |> required "projectName" string
                                                |> required "mapName" xmapNameDecoder
                            "unsubscribedFromView" -> decode WEUnsubscribedFromView
                                                |> required "projectName" string
                                                |> required "viewName" string
                            "viewStatus" -> decode WEViewStatus
                                                |> required "projectName" string
                                                |> required "view" viewDecoder
                                                |> required "maps" (list xNamedMapDecoder)
                            "info" -> decode WEInfo
                                                |> required "info" string
                            "error" -> decode WEError
                                                |> required "error" string

                            otherwise -> fail ("webEvent type " ++ t ++ " not recognized")
    in decodeType decodeFromType