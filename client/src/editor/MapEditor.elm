module MapEditor exposing (mapEditorContent)

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
import ModelUpdate exposing (..)


mapEditorContent : Model -> ProjectModel -> List(Html Msg)
mapEditorContent model pm =
    let  xmapEditorModel = model.xmapEditorModel
    in [
           Grid.grid [ Grid.noSpacing ]
              [ cell 2 2 1 [ mapDialogMapList pm.project]
              , cell 3 5 1 [ mapDialogTextArea model pm]
              , cell 3 5 2 [ mapDialogTable xmapEditorModel.xmapToEdit ]
           ],
           Grid.grid [ Grid.noSpacing]
              [ cell 3 4 1 [ newMapButton model pm ]
              , cell 2 3 1 [ buttonClick model 8 "To Table >" (Internal MapToTable) ]
              , cell 2 3 1 [ buttonClick model 9 "< To Text" (Internal MapToTextArea) ]
              , cell 1 2 1 [ buttonMaybe model 10 "Store" (Maybe.map2 (storeMap pm) xmapEditorModel.xmapName xmapEditorModel.xmapToEdit)   ]
          ]
  ]

newMapButton : Model -> ProjectModel -> Html Msg
newMapButton model pm =
    let xmapEditorModel = model.xmapEditorModel
        storeNewMap = case xmapNameFromString xmapEditorModel.newXmapName of
                          Ok mapName -> Maybe.map (storeMap pm mapName) xmapEditorModel.xmapToEdit
                          Err e -> Just (Internal (ShowMessage e))
    in Grid.grid [ Grid.noSpacing]
        [ cell 4 6 2 [ Textfield.render Mdl [9] model.mdl
                                             [ Textfield.label "New map name"
                                             , Textfield.floatingLabel
                                             , Textfield.text_
                                             , Options.onInput (\s -> Internal (NewMapName s))
                                             ]
                                             [] ]
        , cell 4 6 2 [ buttonMaybe model 7 "New Map" storeNewMap]
           ]


mapDialogMapList : Project -> Html Msg
mapDialogMapList p =
    let listItem mn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Send (WRLoadMaps p.projectName [mn])) ]
                               [ Lists.avatarIcon "list" [], text (xmapNameToString mn) ]
                           ]
    in Lists.ul [] (List.map listItem (fileSourcesOfProject p))

storeMap : ProjectModel -> XMapName -> XMap -> Msg
storeMap pm n m =  WRStoreMap  pm.project.projectName { xmapName = n , xmap = m } |> Send

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
                              , Textfield.value (Maybe.withDefault "" model.xmapEditorModel.xmapEditing)
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
mapHeader = Table.thead [Options.css "display" "table"]
                     [ Table.tr []
                        [
                        Table.th [] [ text "Ids" ],
                        Table.th [] [ text "Values" ]
                        ]
                     ]

mapRows : XMap -> Html Msg
mapRows m = let rows = List.map lineToTableRow (mapToTransposedMatrix m)
            in Table.tbody [Options.css "height" "30vh", Options.css "overflow-y" "auto", Options.css "display" "block" ] rows

lineToTableRow : List String  -> Html Msg
lineToTableRow line = Table.tr [] (List.map (\v ->Table.td [] [ text v ]) line)



