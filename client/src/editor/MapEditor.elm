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
import Material.Options as Options exposing (css)
import List.Extra as ListX exposing (transpose, find)

import ProjectModel exposing (..)
import XMapTypes exposing(..)
import MapsExtraction exposing (..)
import Project exposing (..)
import WebMessages exposing (WebRequest(..))

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
                                      [ Grid.cell [ Grid.size Grid.Tablet 2, Grid.size Grid.Desktop 2, Grid.size Grid.Phone 1, Grid.stretch]
                                          [ mapDialogMapList pm.project]
                                      , Grid.cell [ Grid.size Grid.Tablet 3, Grid.size Grid.Desktop 5, Grid.size Grid.Phone 1, Grid.stretch]
                                          [ mapDialogTextArea model pm]
                                      , Grid.cell [ Grid.size Grid.Tablet 3, Grid.size Grid.Desktop 5, Grid.size Grid.Phone 2, Grid.stretch]
                                          [ mapDialogTable model.xmapToEdit ]
                                  ]
                          ]

mapDialogMapList : Project -> Html Msg
mapDialogMapList p =
    let listItem mn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Send (WRLoadMaps p.projectName [mn])) ]
                               [ Lists.avatarIcon "view_comfy" [], text (xmapNameToString mn) ]
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
                              ]
                              []

mapDialogTable : Maybe XNamedMap -> Html Msg
mapDialogTable mm = case mm of
                        Just m -> Table.table []
                            [
                                mapHeader,
                                mapRows m.xmap
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
mapRows m = let matrix = ListX.transpose (mapToTable m)
                rows = List.map lineToTableRow (Debug.log "Matrix: " matrix)
            in Table.tbody [] rows

lineToTableRow : List String  -> Html Msg
lineToTableRow line = Table.tr [] (List.map (\v ->Table.td [] [ text v ]) line)

mapToTable : XMap -> List (List String)
mapToTable m = [Dict.keys (mapValues m), Dict.values (mapValues m)]

