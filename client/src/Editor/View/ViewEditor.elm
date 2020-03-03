module Editor.View.ViewEditor exposing (viewViewsEditor)

import Editor.View.ViewEditorTable exposing (viewEditorTable)
import Html exposing (Html, div, text)
import Models.InternalMessages exposing (..)
import Transform.MapsExtraction exposing (xmapNameToString)
import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Material.Options as Options exposing (styled)
import Material.TextField as TextField
import Display.MdcIndexes exposing (..)
import Display.NameDialog exposing (nameDialog)
import Transform.NameParser exposing (..)
import Types.Project exposing (..)
import Models.ProjectModel exposing (..)
import Display.UIWrapper exposing (..)
import Types.Views exposing (..)
import Models.WebMessages exposing (WebRequest(..))

viewViewsEditor : Model -> ProjectModel -> Html Msg
viewViewsEditor model pm =
            LayoutGrid.view [ heightInView model.ui.heights.viewViewsEditor ]
                [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span2Desktop, LayoutGrid.span1Phone] [ viewEditorViewsList model pm.project, newViewButton model ]
                , LayoutGrid.cell [LayoutGrid.span6Tablet, LayoutGrid.span10Desktop, LayoutGrid.span3Phone] [ viewEditorForView model pm ]
                ]

title : Model -> String
title model =
    case model.viewEditorModel.viewName of
        Just viewName -> "Editing: " ++ viewName
        Nothing -> "View Editor"

viewEditorForView model pm =
    case model.viewEditorModel.viewName of
        Just vn ->
            LayoutGrid.view [ heightInView model.ui.heights.viewEditorForView ]
                    [ LayoutGrid.cell [LayoutGrid.span6Tablet, LayoutGrid.span8Desktop, LayoutGrid.span3Phone]
                            [ viewEditorTable model model.viewEditorModel.viewToEdit ]
                    , LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span4Desktop, LayoutGrid.span1Phone]
                            [ viewEditorMapList model, addLabelButton model, storeButton model pm]
                    ]
        Nothing -> div [] []

storeButton : Model -> ProjectModel -> Html Msg
storeButton model pm =
    storeView pm model.viewEditorModel |> buttonClick model (makeIndex viewEditorIdx "btnStr") "Store View"

storeView : ProjectModel -> ViewEditorModel -> Msg
storeView pm vm =
    let
        viewEditRowToViewRow (ViewEditRow items) = List.map (\i -> i.content) items |> ViewRow
        viewToStore v vn  = { viewName = vn
                            , rows = List.map viewEditRowToViewRow v.rows
                            }
        storeValidViewToEdit v vn =
                if vm.isNew then
                    Send (WRAddView pm.project.projectName (viewToStore v vn))
                else
                    Send (WRUpdateView pm.project.projectName (viewToStore v vn))
        storeViewWithName vn = case vm.viewToEdit of
            Just v -> storeValidViewToEdit v vn
            Nothing -> Internal (ShowMessage "Please fill the view")
    in
        case vm.viewName of
            Just vn -> storeViewWithName vn
            Nothing -> Internal (ShowMessage "Please enter a view name")


viewEditorViewsList : Model -> Project -> Html Msg
viewEditorViewsList model p =
    let
        sendLoadView index = sendListMsg (\vn -> (Send (WRLoadView p.projectName vn))) p.viewNames index
        listItem vn =
            Lists.li []
                [
                    Lists.graphicIcon [] "list"
                    , text vn
                ]
    in
      div []
        [
            titleWithIcon (title model) "view_module" "Pink",
            Lists.ul Mdc (makeIndex viewEditorIdx "lstVew") model.mdc
                ((Lists.onSelectListItem sendLoadView) :: (scrollableListStyle model.ui.heights.viewEditorViewsList))
                (List.map listItem p.viewNames)
        ]

viewEditorMapList : Model -> Html Msg
viewEditorMapList model =
    let
        buildMsg mn = Internal (AddItemToView model.viewEditorModel.rowToAddTo (MapItem mn))
        selectItem index = Internal (SelectMapIndexForView index)
        sendAddItem = Maybe.withDefault None (Maybe.map (\idx -> sendListMsg buildMsg model.mapsInProject idx) model.viewEditorModel.selectedMapIdx)
        listItem mn =
            Lists.li []
                [
                    Lists.graphicIcon [] "list" ,
                    text (xmapNameToString mn)
                ]
    in
        div []
            [
                Lists.ul Mdc (makeIndex viewEditorIdx "lstMap") model.mdc
                    ((Lists.onSelectListItem selectItem) :: (scrollableListStyle model.ui.heights.viewEditorMapList))
                    (List.map listItem model.mapsInProject)
            , buttonClick model (makeIndex viewEditorIdx "btnAddMap") "Add map"  sendAddItem
            ]

newViewButton : Model -> Html Msg
newViewButton model =
    let
        newViewMessage =
            case nameFromString model.viewEditorModel.newViewName of
                Ok newViewName -> Internal (NewViewWithName newViewName)
                Err err -> Internal (CloseDialogWithError err)
        idxDialog = makeIndex viewEditorIdx "dlgNewVew"
    in
        div []
            [
              nameDialog idxDialog model "New view" (\s -> Internal (UpdateViewName s)) newViewMessage
            , buttonClick model (makeIndex viewEditorIdx "btnNewVew") "New view"  (Internal (ShowDialog idxDialog))
            ]


addLabelButton : Model -> Html Msg
addLabelButton model =
    let
        viewEditorModel = model.viewEditorModel
        newLabelMessage = Internal (AddItemToView viewEditorModel.rowToAddTo (LabelItem viewEditorModel.labelEditing))
    in
        LayoutGrid.inner []
                    [ LayoutGrid.cell [LayoutGrid.span4Tablet, LayoutGrid.span7Desktop, LayoutGrid.span2Phone]
                        [ TextField.view Mdc
                            (makeIndex viewEditorIdx "txtAddLbl")
                            model.mdc
                            [ TextField.label "Label text"
                            , TextField.value viewEditorModel.labelEditing
                            , Options.onInput (\s -> Internal (UpdateViewLabel s))
                            ]
                            []
                            ]
                    , LayoutGrid.cell [LayoutGrid.span4Tablet, LayoutGrid.span5Desktop, LayoutGrid.span2Phone] [
                        buttonClick model (makeIndex viewEditorIdx "btnAddLbl") "Add label" newLabelMessage
                     ]
            ]


