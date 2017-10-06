module ViewUI exposing (viewView)

import Html        exposing (..)
import Html.Attributes exposing (class)
import Material.Table as Table
import Set as Set
import Dict as Dict
import List.Extra exposing (transpose)

import ProjectModel exposing (..)
import Views exposing (..)
import XMapTypes exposing (..)
import MapsExtraction exposing (..)

viewView : Model -> ProjectModel -> ViewModel -> Html Msg
viewView model pm vm = div [class  "content"]
                            (List.map (\row -> viewRow vm row) vm.view.rows)

viewRow : ViewModel -> ViewRow -> Html Msg
viewRow vm row = Table.table []
                    [
                        viewRowHeader row,
                        viewRowBody vm row
                    ]

viewRowHeader : ViewRow -> Html Msg
viewRowHeader row =
    let header id = Table.th [] [ text id ]
    in Table.thead []
                     [ Table.tr []
                        (List.map header ("Ids" :: rowNames row))
                     ]

viewRowBody : ViewModel -> ViewRow -> Html Msg
viewRowBody vm row = let matrix = transpose (rowToTable row vm)
                         rows = List.map rowLineToTableRow (Debug.log "Matrix: " matrix)
                     in Table.tbody [] rows

rowLineToTableRow : List String  -> Html Msg
rowLineToTableRow line = Table.tr [] (List.map (\v ->Table.td [] [ text v ]) line)

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
    let xmapNameToString l = String.join "/" l
        name item = case item of
            MapItem xmapName -> xmapNameToString xmapName
            LabelItem label -> label
    in List.map name items


