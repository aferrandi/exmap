module MapEditor exposing (..)

import Dict as Dict
import Html        exposing (..)
import Html.Events exposing (onClick)
import Material
import Material.Button as Button exposing (render)
import Material.Dialog as Dialog exposing (openOn)
import Material.Textfield as Textfield
import Material.Grid as Grid
import Material.List as Lists
import Material.Table as Table
import Material.Icon as Icon
import Material.Options as Options exposing (css)
import List.Extra as ListX exposing (transpose, find)

import ProjectModel exposing (..)
import XMapTypes exposing(..)
import MapsExtraction exposing (..)
import Project exposing (..)
import WebMessages exposing (WebRequest(..))
import Stretch exposing (..)
import XMapText exposing (..)
import XMapParse exposing (..)
import UIWrapper exposing (..)

viewModel : Model -> ProjectModel -> Html Msg
viewModel model pm =
                Dialog.view
                    [ Options.css "width" "50%" ]
                    [ Dialog.title [] [ text "Map Editor" ]
                    , Dialog.content [] (mapDialogContent model pm)
                    , Dialog.actions []
                        [ Button.render Mdl
                            [ 0 ]
                            model.mdl
                            [ Dialog.closeOn "click" ]
                            [ text "Close" ]
                        ]
                    ]

mapDialogContent : Model -> ProjectModel -> List(Html Msg)
mapDialogContent model pm = [
                                   Grid.grid [ Grid.noSpacing]
                                      [ cell 2 2 1 [ mapDialogMapList pm.project]
                                      , cell 3 5 1 [ mapDialogTextArea model pm]
                                      , cell 3 5 2 [ mapDialogTable model.xmapToEdit ]
                                   ],
                                   Grid.grid [ Grid.noSpacing]
                                      [ cell 2 3 1 [ buttonClick model 7 "New Map" (Internal MapToTable) ]
                                      , cell 2 3 1 [ buttonClick model 8 "To Table >" (Internal MapToTable) ]
                                      , cell 2 3 1 [ buttonClick model 9 "< To Text" (Internal MapToTextArea) ]
                                      , cell 2 3 1 [ buttonMaybe model 10 "Store" (Maybe.map2 (\n m -> Send (WRStoreMap  pm.project.projectName { xmapName = n , xmap = m }) ) model.xmapName model.xmapToEdit)   ]
                                  ]
                          ]



mapDialogMapList : Project -> Html Msg
mapDialogMapList p =
    let listItem mn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Send (WRLoadMaps p.projectName [mn])) ]
                               [ Lists.avatarIcon "list" [], text (xmapNameToString mn) ]
                           ]
    in Lists.ul [] (List.map listItem (fileSourcesOfProject p))

fileSourcesOfProject : Project -> List XMapName
fileSourcesOfProject p =
    let maybeMaps : Maybe (List XMapName)
        maybeMaps = ListX.find (\s -> s.sourceType == FileSource) p.sources |> Maybe.map (\s -> s.sourceOfMaps)
    in Maybe.withDefault [] maybeMaps

mapDialogTextArea : Model -> ProjectModel -> Html Msg
mapDialogTextArea model pm = Textfield.render Mdl [9] model.mdl
                              [ Textfield.label "Enter the map data"
                              , Textfield.floatingLabel
                              , Textfield.textarea
                              , Textfield.rows 20
                              , Textfield.value (Maybe.withDefault "" model.xmapEditing)
                              , Options.onInput (\s -> Internal (TextToTextArea s))
                              ]
                              []

mapDialogTable : Maybe XMap -> Html Msg
mapDialogTable mm = case mm of
                        Just m -> Table.table []
                            [
                                mapHeader,
                                mapRows m
                            ]
                        Nothing -> Table.table []
                            [
                                mapHeader,
                                Table.tbody [] []
                            ]

mapHeader : Html Msg
mapHeader = Table.thead []
                     [ Table.tr []
                        [
                        Table.th [] [ text "Ids" ],
                        Table.th [] [ text "Values" ]
                        ]
                     ]

mapRows : XMap -> Html Msg
mapRows m = let rows = List.map lineToTableRow (mapToTransposedMatrix m)
            in Table.tbody [] rows

lineToTableRow : List String  -> Html Msg
lineToTableRow line = Table.tr [] (List.map (\v ->Table.td [] [ text v ]) line)



