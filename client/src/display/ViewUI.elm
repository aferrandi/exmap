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
import List.Extra exposing (uncons)

import ProjectModel exposing (..)
import Views exposing (..)
import XMapTypes exposing (..)

viewView : Model -> ProjectModel -> ViewModel -> Html Msg
viewView model pm vm = Table.table []
                       [ Table.thead []
                         [ Table.tr []
                           [ Table.th [] [ text "Material" ]
                           , Table.th [ ] [ text "Quantity" ]
                           , Table.th [ ] [ text "Unit Price" ]
                           ]
                         ]
                       , Table.tbody []
                           (List.concatMap (viewViewRow model vm) vm.view.rows)
                       ]

rowLineToTableRow : List String  -> Html Msg
rowLineToTableRow line = Table.tr [] (List.map (\v ->Table.td [] [ text v ]) line)

viewViewRow : Model -> ViewModel -> ViewRow -> List (Html Msg)
viewViewRow model vm row = let matrix = transpose (rowToTable row vm)
                           in List.map rowLineToTableRow matrix

transpose : List (List String) -> List (List String)
transpose rows = let headOr d l = List.head l |> Maybe.withDefault d
                     tailOr d l = List.tail l |> Maybe.withDefault d
                     heads ll = List.map (headOr "") ll
                     tails ll = List.map (tailOr []) ll
                     transpose1 : List (List String) -> List (List String) -> List (List String)
                     transpose1 transposed toTranspose = if List.isEmpty (headOr [] toTranspose)
                                                         then transposed
                                                         else transpose1 (heads toTranspose :: transposed) (tails toTranspose)
                 in transpose1 [[]] rows


rowToTable : ViewRow -> ViewModel -> List (List String)
rowToTable row vm = let ids = rowIds row vm.maps
                        values (ViewRow items) = List.map (itemToTable vm.maps ids) items
                    in Set.toList ids :: values row



itemToTable : XMapByName -> Set.Set XMapKey -> ViewItem -> List String
itemToTable ms ids item = let idsMap = Set.foldr (\id dict -> Dict.insert id  "" dict) Dict.empty ids
                              keyValueToString k v =  toString v
                              mapValuesForEachId xmapName = Dict.values (Dict.union idsMap (mapValues xmapName))
                              mapValues xmapName = case Dict.get xmapName ms of
                                 Just (XMapInt (MapValue mapInt)) -> Dict.map keyValueToString mapInt
                                 Just (XMapString (MapValue mapString)) -> Dict.map keyValueToString mapString
                                 Just (XMapBool (MapValue mapBool)) -> Dict.map keyValueToString mapBool
                                 Just (XMapDouble (MapValue mapDouble)) -> Dict.map keyValueToString mapDouble
                                 Nothing -> idsMap
                          in case item of
                             MapItem xmapName -> mapValuesForEachId xmapName
                             LabelItem label -> List.singleton label

mapKeys : XMapName -> XMapByName -> List XMapKey
mapKeys xmapName ms = case Dict.get xmapName ms of
                       Just (XMapInt (MapValue mapInt)) -> Dict.keys  mapInt
                       Just (XMapString (MapValue mapString)) -> Dict.keys  mapString
                       Just (XMapBool (MapValue mapBool)) -> Dict.keys  mapBool
                       Just (XMapDouble (MapValue mapDouble)) -> Dict.keys  mapDouble
                       Nothing -> []


rowIds : ViewRow -> XMapByName -> Set.Set XMapKey
rowIds (ViewRow items) ms = let keysForMap item = case item of
                                                     MapItem xmapName -> Set.fromList (mapKeys xmapName ms)
                                                     LabelItem _ -> Set.empty
                            in List.map keysForMap items |> List.foldr Set.union Set.empty