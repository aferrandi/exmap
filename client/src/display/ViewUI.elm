module ViewUI exposing (viewView)

import Html        exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Scheme
import Material.Icon as Icon
import Material.Button as Button
import Material.Table as Table
import Material.Options as Options exposing (css)
import Set as Set
import Dict as Dict
import List.Extra exposing (uncons, transpose)

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
viewRowHeader row = Table.thead []
                     [ Table.tr []
                       [ Table.th [] [ text "Ids" ]
                       , Table.th [ ] [ text "Quantity" ]
                       , Table.th [ ] [ text "Unit Price" ]
                       ]
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
rowIds (ViewRow items) ms = let keysForMap item = case item of
                                                     MapItem xmapName -> case Dict.get xmapName ms of
                                                                            Just m -> Set.fromList (mapKeys m)
                                                                            Nothing -> Set.empty
                                                     LabelItem _ -> Set.empty
                            in List.map keysForMap items |> List.foldr Set.union Set.empty