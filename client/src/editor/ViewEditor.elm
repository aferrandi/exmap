module ViewEditor exposing (viewViewEditor)

import Html exposing (Html,text, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class, style)
import Material.Tabs as Tabs
import Material.Icon as Icon
import Material.Dialog as Dialog
import Material.List as Lists
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Table as Table
import Material.Menu as Menu exposing (Item)
import Material.Grid as Grid exposing (Device(..))
import Material.Options as Options exposing (css)
import List.Extra as ListX exposing (transpose)

import Project exposing (..)
import Views exposing (..)
import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import InternalMessages exposing (..)
import ViewUI exposing (..)
import UIWrapper exposing (..)
import Stretch exposing (..)
import MapsExtraction exposing (xmapNameToString)


viewViewEditor : Model -> ProjectModel -> Html Msg
viewViewEditor model pm = div [] [
                                titleWithIcon "View Editor" "view_module" Color.Pink,
                                Grid.grid [heightInView 70]
                                [ cell 2 2 1 [ Color.background lighterGrey]  [viewsList pm.project]
                                , cell 6 10 3 [] [ viewEditorForView model pm ]
                                 ]
                                ]

viewEditorForView model pm = case model.viewEditorModel.viewName of
                               Just vn -> div [] [
                                     titleWithIcon ("View " ++ Maybe.withDefault "" model.viewEditorModel.viewName) "view_module" Color.Pink,
                                     Grid.grid [heightInView 70]
                                     [
                                     cell 6 10 3 [] [ viewEditorTable model.viewEditorModel.viewToEdit ]
                                     , cell 2 2 1 [ Color.background lighterGrey]  [viewEditorMapList model ]
                                      ]
                                    ]
                               Nothing -> div [] []

viewsList : Project -> Html Msg
viewsList p =
    let listItem vn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Send (WRLoadView p.projectName vn)) ]
                               [ Lists.avatarIcon "list" [], text vn ]                           ]
    in Lists.ul [] (List.map listItem p.viewNames)




viewEditorTable : Maybe View -> Html Msg
viewEditorTable mv = case mv of
                        Just v -> Table.table []
                            [ viewRows v ]
                        Nothing -> Table.table []
                            [ Table.tbody [] [] ]
viewRows : View -> Html Msg
viewRows v = let rows = List.map viewRowToTableCells v.rows |> List.map (Table.tr [])
             in Table.tbody [] rows

viewCell : ViewItem -> Html Msg
viewCell i = case i of
                MapItem mn -> Table.td [Color.background (pastel Color.DeepOrange)] [ text (xmapNameToString mn) ]
                LabelItem l -> Table.td [Color.background (pastel Color.LightBlue)] [ text l ]

viewRowToTableCells : ViewRow -> List (Html Msg)
viewRowToTableCells (ViewRow row) = List.map viewCell row

viewEditorMapList : Model -> Html Msg
viewEditorMapList model =
    let listItem mn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Internal (AddItemToView 0 (MapItem mn))) ]
                               [ Lists.avatarIcon "list" [], text (xmapNameToString mn) ]
                           ]
    in Lists.ul [] (List.map listItem model.mapsInProject)
