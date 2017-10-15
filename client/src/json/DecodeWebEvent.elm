module DecodeWebEvent exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import WebMessages exposing (..)
import DecodeProject exposing (..)

webEventDecoder : Decoder WebEvent
webEventDecoder =
    let decodeFromType t = case t of
                            "allProjects" -> decode WEAllProjects
                                                |> required "projectNames" allProjectsDecoder
                            "viewChanged" -> decode WEViewChanged
                                                |> required "projectName" string
                                                |> required "viewName" string
                                                |> required "maps" (list xNamedMapDecoder)
                            "projectContent" -> decode WEProjectContent
                                                |> required "project" projectDecoder
                            "projectStored" -> decode WEProjectStored
                                                |> required "projectName" string
                            "mapLoaded" -> decode WEMapsLoaded
                                                |> required "projectName" string
                                                |> required "maps"(list xNamedMapDecoder)
                            "mapStored" -> decode WEMapStored
                                                |> required "projectName" string
                                                |> required "mapName" xmapNameDecoder
                            "unsubscribedFromView" -> decode WEUnsubscribedFromView
                                                |> required "projectName" string
                                                |> required "viewName" string
                            "mapsInProject" -> decode WEMapsInProject
                                                |> required "projectName" string
                                                |> required "mapNames"(list xmapNameDecoder)
                            "viewStatus" -> decode WEViewStatus
                                                |> required "projectName" string
                                                |> required "view" viewDecoder
                                                |> required "maps" (list xNamedMapDecoder)
                            "viewLoaded" -> decode WEViewLoaded
                                                |> required "projectName" string
                                                |> required "view"viewDecoder
                            "viewStored" -> decode WEViewStored
                                                |> required "projectName" string
                                                |> required "viewName" string
                            "calculationLoaded" -> decode WECalculationLoaded
                                                |> required "projectName" string
                                                |> required "calculationName" string
                                                |> required "calculationFormulaText" string
                            "calculationStored"  -> decode WECalculationStored
                                                |> required "projectName" string
                                                |> required "calculationName" string
                            "info" -> decode WEInfo
                                                |> required "info" string
                            "error" -> decode WEError
                                                |> required "error" string

                            _ -> fail ("webEvent type " ++ t ++ " not recognized")
    in decodeType decodeFromType