module Json.DecodeWebEvent exposing (..)

import Json.DecodeCalculation exposing (..)
import Json.DecodeProject exposing (..)
import Json.DecodeView exposing (..)
import Json.DecodeXMap exposing (..)
import Json.Decode exposing (..)
import Models.WebMessages exposing (..)


webEventDecoder : Decoder WebEvent
webEventDecoder =
    let
        decodeFromType t =
            case t of
                "allProjects" ->
                    map WEAllProjects
                        (field "projectNames" allProjectsDecoder)
                "viewChanged" ->
                    map3 WEViewChanged
                        (field "projectName" string)
                        (field "viewName" string)
                        (field "maps" (list xNamedMapDecoder))
                "projectContent" ->
                    map WEProjectContent
                        (field "project" projectDecoder)
                "projectStored" ->
                    map WEProjectStored
                        (field "project" projectDecoder)
                "mapLoaded" ->
                    map2 WEMapLoaded
                        (field "projectName" string)
                        (field "map" xNamedMapDecoder)
                "mapAdded" ->
                    map3 WEMapAdded
                        (field "projectName" string)
                        (field "mapName" xmapNameDecoder)
                        (field "size" int)
                "mapUpdated" ->
                    map3 WEMapUpdated
                        (field "projectName" string)
                        (field "mapName" xmapNameDecoder)
                        (field "size" int)
                "unsubscribedFromView" ->
                    map2 WEUnsubscribedFromView
                        (field "projectName" string)
                        (field "viewName" string)
                "mapsInProject" ->
                    map2 WEMapsInProject
                        (field "projectName" string)
                        (field "mapNames" (list xmapDefinitionDecoder))
                "viewStatus" ->
                    map3 WEViewStatus
                        (field "projectName" string)
                        (field "view" viewDecoder)
                        (field "maps" (list xNamedMapDecoder))
                "viewLoaded" ->
                    map2 WEViewLoaded
                        (field "projectName" string)
                        (field "view" viewDecoder)
                "viewAdded" ->
                    map2 WEViewAdded
                        (field "projectName" string)
                        (field "viewName" string)
                "viewUpdated" ->
                    map2 WEViewUpdated
                        (field "projectName" string)
                        (field "viewName" string)
                "calculationLoaded" ->
                    map2 WECalculationLoaded
                        (field "projectName" string)
                        (field "calculationSource" calculationSourceDecoder)
                "calculationAdded" ->
                    map2 WECalculationAdded
                        (field "projectName" string)
                        (field "calculationName" string)
                "calculationUpdated" ->
                    map2 WECalculationUpdated
                        (field "projectName" string)
                        (field "calculationName" string)
                "functions" ->
                    map WEFunctions
                        (field "functions" functionsDecoder)
                "info" ->
                    map WEInfo
                        (field "info" string)
                "error" ->
                    map WEError
                        (field "error" string)
                _ ->
                    fail ("webEvent type " ++ t ++ " not recognized")
    in
    decodeType decodeFromType
