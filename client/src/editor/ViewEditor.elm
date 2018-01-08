module ViewEditor exposing (viewViewsEditor)

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
import Material.Textfield as Textfield
import Material.Menu as Menu exposing (Item)
import Material.Grid as Grid exposing (Device(..))
import Material.Options as Options exposing (css)
import Material.Toggles as Toggles
import List.Extra as ListX exposing (transpose)
import Set as Set

import Project exposing (..)
import Views exposing (..)
import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import InternalMessages exposing (..)
import NameParser exposing (..)
import ViewUI exposing (..)
import UIWrapper exposing (..)
import MapsExtraction exposing (xmapNameToString)
import MdlIndexes exposing (..)


viewViewsEditor : Model -> ProjectModel -> Html Msg
viewViewsEditor model pm = let title = case model.viewEditorModel.viewName of
                                            Just viewName -> "Editing view: " ++ viewName
                                            Nothing -> "View Editor"
                           in div [] [
                                titleWithIcon title "view_module" Color.Pink,
                                Grid.grid [heightInView 70]
                                [ cell 2 2 1 [ ]  [viewsList pm.project, newViewButton model]
                                , cell 6 10 3 [] [ viewEditorForView model pm ]
                                 ]
                                ]

viewEditorForView model pm = case model.viewEditorModel.viewName of
                               Just vn -> div [] [
                                     Grid.grid [heightInView 60]
                                     [ cell 6 10 3 [] [ viewEditorTable model model.viewEditorModel.viewToEdit ]
                                     , cell 2 2 1 [ ]  [viewEditorMapList model, addLabelButton model ]
                                     ],
                                     Grid.grid [ Grid.noSpacing]
                                          [ cell 2 6 4 [] [ ]
                                          , cell 1 3 2 [] [ addRowButton model ]
                                          , cell 1 3 2 [] [ storeButton model pm  ]
                                     ]
                                    ]
                               Nothing -> div [] []

storeButton : Model -> ProjectModel -> Html Msg
storeButton model pm = storeView pm model.viewEditorModel |> buttonClick model [viewEditorIdx, 1]  "Store"

storeView : ProjectModel -> ViewEditorModel -> Msg
storeView pm vm = case vm.viewName of
    Just vn -> case vm.viewToEdit of
        Just v -> Send (WRStoreView pm.project.projectName {
                                viewName = vn,
                                rows = v.rows})
        Nothing -> Internal (ShowMessage "Please fill the view")
    Nothing -> Internal (ShowMessage "Please enter a view name")


viewsList : Project -> Html Msg
viewsList p =
    let listItem vn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Send (WRLoadView p.projectName vn)) ]
                               [ Lists.avatarIcon "list" [], text vn ]                           ]
    in Lists.ul [heightInView 55, Color.background lighterGrey] (List.map listItem p.viewNames)

viewEditorTable : Model -> Maybe View -> Html Msg
viewEditorTable model mv = case mv of
                        Just v -> Table.table []
                            [ viewRows model v ]
                        Nothing -> Table.table []
                            [ Table.tbody [] [] ]
viewRows : Model -> View -> Html Msg
viewRows model v = let rows = ListX.zip (List.range 0 (List.length v.rows)) v.rows |> List.map (viewRowToTableCells model) |> List.map (Table.tr [])
                   in Table.tbody [] rows

viewChoice model rowI =
    Table.td []
      [
        Toggles.radio Mdl [viewEditorIdx, 2] model.mdl
        [ Toggles.value (model.viewEditorModel.rowToAddTo == rowI)
          , Toggles.group "tableGroup"
          , Toggles.ripple
          , Options.onToggle (Internal (ChangeViewEditSelectedRow rowI))
          ] []
      ]

viewCell : ViewItem -> Html Msg
viewCell i = case i of
                MapItem mn -> Table.td [Color.background (pastel Color.DeepOrange)] [ text (xmapNameToString mn) ]
                LabelItem l -> Table.td [Color.background (pastel Color.LightBlue)] [ text l ]

viewRowToTableCells : Model -> (Int, ViewRow) -> List (Html Msg)
viewRowToTableCells model (rowIdx,  (ViewRow row)) = (viewChoice model rowIdx) :: (List.map viewCell row)

viewEditorMapList : Model -> Html Msg
viewEditorMapList model =
    let listItem mn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Internal (AddItemToView model.viewEditorModel.rowToAddTo (MapItem mn))) ]
                               [ Lists.avatarIcon "list" [], text (xmapNameToString mn) ]
                           ]
    in Lists.ul [Color.background lighterGrey, heightInView 40] (List.map listItem model.mapsInProject)

newViewButton : Model -> Html Msg
newViewButton model =
    let newViewMessage = case nameFromString model.viewEditorModel.newViewName of
                            Ok newViewName -> Internal (NewViewWithName newViewName)
                            Err err -> Internal (ShowMessage err)
    in div []
        [ Textfield.render Mdl [viewEditorIdx, 3] model.mdl
                                             [ Textfield.label "New view name"
                                             , Textfield.floatingLabel
                                             , Textfield.text_
                                             , Options.onInput (\s -> Internal (UpdateViewName s))
                                             ][]
        , buttonClick model [viewEditorIdx, 4] "New view" newViewMessage
        ]

addLabelButton : Model -> Html Msg
addLabelButton model =
    let viewEditorModel = model.viewEditorModel
        newViewMessage = (Internal (AddItemToView viewEditorModel.rowToAddTo (LabelItem viewEditorModel.labelEditing)))
    in div []
        [ Textfield.render Mdl [viewEditorIdx, 5] model.mdl
                                             [ Textfield.label "Label name"
                                             , Textfield.floatingLabel
                                             , Textfield.text_
                                             , Options.onInput (\s -> Internal (UpdateViewLabel s))
                                             ][]
        , buttonClick model [viewEditorIdx, 6] "Add label" newViewMessage
        ]

addRowButton : Model -> Html Msg
addRowButton model = buttonClick model [viewEditorIdx, 7] "Add row" (Internal AddRowToView)

