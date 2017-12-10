module ViewUI exposing (viewView)

import Html exposing (Html, text, div)
import Html.Attributes exposing (class)
import Material.Table as Table
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Color as Color
import Set as Set
import Dict as Dict
import List.Extra exposing (transpose, zip)

import ProjectModel exposing (..)
import Views exposing (..)
import UIWrapper exposing (..)
import XMapTypes exposing (..)
import MapsExtraction exposing (..)

viewView : Model -> ProjectModel -> ViewModel -> Html Msg
viewView model pm vm =  Table.table (scrollableTableStyle 60) (List.concatMap (viewRow vm) vm.view.rows)

viewRow : ViewModel -> ViewRow -> List (Html Msg)
viewRow vm row = [ viewRowHeader row, viewRowBody vm row ]

viewRowHeader : ViewRow -> Html Msg
viewRowHeader  row=
    let cell id = Table.th bold [ text id ]
    in Table.thead []
                     [ Table.tr []
                        (List.map cell ("Ids" :: rowNames row))
                     ]

viewRowBody : ViewModel -> ViewRow -> Html Msg
viewRowBody vm row = let matrix = transpose (rowToTable row vm)
                         rows = List.map rowLineToTableRow matrix
                     in Table.tbody [] rows

rowLineToTableRow : List String -> Html Msg
rowLineToTableRow line =
    let cell v = Table.td [] [ text v ]
    in Table.tr [] (List.map cell line)

rowToTable : ViewRow -> ViewModel -> List (List String)
rowToTable row vm = let ids = rowIds row vm.maps
                        values (ViewRow items) = List.map (itemToTable vm.maps ids) items
                    in Set.toList ids :: values row

rowIds : ViewRow -> XMapByName -> Set.Set XMapKey
rowIds (ViewRow items) ms =
    let keysForMap item = case item of
            MapItem xmapName -> case Dict.get xmapName ms of
                                Just m -> Set.fromList (mapKeys m)
                                Nothing -> Set.empty
            LabelItem _ -> Set.empty
    in List.map keysForMap items |> List.foldr Set.union Set.empty

rowNames : ViewRow -> List String
rowNames (ViewRow items) =
    let name item = case item of
            MapItem xmapName -> xmapNameToString xmapName
            LabelItem label -> label
    in List.map name items

itemToTable : XMapByName -> Set.Set XMapKey -> ViewItem -> List String
itemToTable ms ids item =
    let idsMap = Set.foldr (\id dict -> Dict.insert id  "" dict) Dict.empty ids
        mapValuesInDict xmapName = case Dict.get xmapName ms of
            Just m -> mapValues m
            Nothing -> idsMap
        mapValuesForEachId xmapName = Dict.values (Dict.union (mapValuesInDict xmapName) idsMap)
      in case item of
         MapItem xmapName -> mapValuesForEachId xmapName
         LabelItem _ -> Dict.values idsMap