module MapEditor exposing (mapEditorView)

import Dict as Dict
import Html        exposing (..)
import Html.Events exposing (onClick)
import Material
import Material.Button as Button exposing (render)
import Material.Textfield as Textfield
import Material.Grid as Grid
import Material.List as Lists
import Material.Table as Table
import Material.Color as Color
import Material.Icon as Icon
import Material.Options as Options exposing (css)
import List.Extra as ListX exposing (transpose, find)

import ProjectModel exposing (..)
import XMapTypes exposing(..)
import MapsExtraction exposing (..)
import Project exposing (..)
import WebMessages exposing (WebRequest(..))
import InternalMessages exposing (..)
import XMapText exposing (..)
import XMapParse exposing (..)
import UIWrapper exposing (..)
import ModelUpdate exposing (..)
import MdlIndexes exposing (..)


mapEditorView : Model -> ProjectModel -> Html Msg
mapEditorView model pm =
    let  xmapEditorModel = model.xmapEditorModel
         title = case xmapEditorModel.xmapName of
                        Just xmapName -> "Editing map: " ++ xmapNameToString xmapName
                        Nothing -> "Map Editor"
    in div [] [
           titleWithIcon title "layers" Color.DeepOrange,
           Grid.grid [ Grid.noSpacing, heightInView 70 ]
              [ cell 2 2 1 [] [ mapEditorMapList pm.project, newMapButton model]
              , cell 6 10 3 [] [ mapEditorViewForMap model pm]
           ]
  ]

mapEditorViewForMap : Model -> ProjectModel -> Html Msg
mapEditorViewForMap model pm =
    let  xmapEditorModel = model.xmapEditorModel
    in case model.xmapEditorModel.xmapName of
        Just mn -> div [] [
               Grid.grid [ Grid.noSpacing, heightInView 60 ]
                  [ cell 3 5 1 [] [ mapEditorTextArea model pm]
                  , cell 5 7 3 [] [ mapEditorTable xmapEditorModel.xmapToEdit ]
               ],
               Grid.grid [ Grid.noSpacing]
                  [ cell 1 4 3 [] [ buttonClick model [mapEditorIdx, 1] "To Table >" (Internal MapToTable) ]
                  , cell 2 4 3 [] [ buttonClick model [mapEditorIdx, 2] "< To Text" (Internal MapToTextArea) ]
                  , cell 1 4 2 [] [ buttonMaybe model [mapEditorIdx, 3] "Store" (Maybe.map2 (storeMap pm) xmapEditorModel.xmapName xmapEditorModel.xmapToEdit)   ]
              ]
            ]
        Nothing -> div [][]



xmapTypeChoice : Model  -> Html Msg
xmapTypeChoice  model =
  let hasType t = model.xmapEditorModel.xmapType == t
  in div []
  [
    toggle model [mapEditorIdx, 4] "Double" "mapType" (hasType TypeDouble) (Internal (ChangeMapType TypeDouble)),
    toggle model [mapEditorIdx, 5] "Int" "mapType" (hasType TypeInt) (Internal (ChangeMapType TypeInt)),
    toggle model [mapEditorIdx, 6] "String" "mapType" (hasType TypeString) (Internal (ChangeMapType TypeString)),
    toggle model [mapEditorIdx, 7] "Bool" "mapType" (hasType TypeBool) (Internal (ChangeMapType TypeBool))
    ]


newMapButton : Model -> Html Msg
newMapButton model=
    let xmapEditorModel = model.xmapEditorModel
        storeNewMap = case xmapNameFromString xmapEditorModel.newXmapName of
                          Ok mn -> Internal (NewMapWithName mn xmapEditorModel.xmapType)
                          Err e -> Internal (ShowMessage e)
    in div[] [
        xmapTypeChoice model,
        Textfield.render Mdl [mapEditorIdx, 8] model.mdl
                                             [ Textfield.label "New map name"
                                             , Textfield.floatingLabel
                                             , Textfield.text_
                                             , Options.onInput (\s -> Internal (UpdateMapName s))
                                             ]
                                             [],
        buttonClick model [mapEditorIdx, 9] "New map" storeNewMap
           ]


mapEditorMapList : Project -> Html Msg
mapEditorMapList p =
    let listItem mn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Internal (ShowMapInEditor mn)) ]
                               [ Lists.avatarIcon "list" [], text (xmapNameToString mn) ]
                           ]
    in Lists.ul [heightInView 50, Color.background lighterGrey] (List.map listItem (fileSourcesOfProject p))

storeMap : ProjectModel -> XMapName -> XMap -> Msg
storeMap pm n m =  WRStoreMap  pm.project.projectName { xmapName = n , xmap = m } |> Send

fileSourcesOfProject : Project -> List XMapName
fileSourcesOfProject p =
    let maybeMaps : Maybe (List XMapName)
        maybeMaps = ListX.find (\s -> s.sourceType == FileSource) p.sources |> Maybe.map (\s -> s.sourceOfMaps)
    in Maybe.withDefault [] maybeMaps

mapEditorTextArea : Model -> ProjectModel -> Html Msg
mapEditorTextArea model pm = Textfield.render Mdl [mapEditorIdx, 10] model.mdl
                              [ Textfield.label "Enter the map data"
                              , Textfield.floatingLabel
                              , Textfield.textarea
                              , Textfield.rows 20
                              , Textfield.value (Maybe.withDefault "" model.xmapEditorModel.xmapEditing)
                              , Options.onInput (\s -> Internal (TextToMapTextArea s))
                              ]
                              []

mapEditorTableFull  : XMap -> Html Msg
mapEditorTableFull m = Table.table (scrollableTableStyle 50) [
                                                           mapHeader,
                                                           mapRows m
                                                           ]

mapEditorTableEmpty : Html Msg
mapEditorTableEmpty = Table.table [] [
                                        mapHeader,
                                        Table.tbody [] []
                                        ]

mapEditorTable : Maybe XMap -> Html Msg
mapEditorTable mm = case mm of
                        Just m -> mapEditorTableFull m
                        Nothing -> mapEditorTableEmpty

mapHeader : Html Msg
mapHeader = Table.thead []
                     [ Table.tr []
                        [
                        Table.th bold [ text "Ids" ],
                        Table.th bold [ text "Values" ]
                        ]
                     ]

mapRows : XMap -> Html Msg
mapRows m = let rows = List.map lineToTableRow (mapToTransposedMatrix m)
            in Table.tbody [] rows

lineToTableRow : List String  -> Html Msg
lineToTableRow line = Table.tr [] (List.map (\v ->Table.td [] [ text v ]) line)




