module Editor.ViewEditor exposing (viewViewsEditor)

import Html exposing (Html, div, text)
import Models.InternalMessages exposing (..)
import List.Extra as ListX
import Dict as Dict

import Transform.MapsExtraction exposing (xmapNameToString)

import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Material.Options as Options
import Material.DataTable as DataTable
import Material.TextField as TextField
import Material.RadioButton as RadioButton
import Material.Checkbox as Checkbox
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
            LayoutGrid.view [ heightInView 70 ]
                [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span2Desktop, LayoutGrid.span1Phone] [ viewsList model pm.project, newViewButton model ]
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
            div []
                [ LayoutGrid.view [ heightInView 55 ]
                    [ LayoutGrid.cell [LayoutGrid.span6Tablet, LayoutGrid.span9Desktop, LayoutGrid.span3Phone]
                            [ viewEditorTable model model.viewEditorModel.viewToEdit ]
                    , LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span3Desktop, LayoutGrid.span1Phone]
                            [ viewEditorMapList model, addLabelButton model ]
                    ]
                    -- Grid.noSpacing
                , LayoutGrid.view [  ]
                    [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span3Desktop, LayoutGrid.span4Phone] []
                    , LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span3Desktop, LayoutGrid.span2Phone] [ addRowButton model ]
                    , LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span3Desktop, LayoutGrid.span2Phone] [ removeCellsButton model ]
                    , LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span3Desktop, LayoutGrid.span2Phone] [ storeButton model pm ]
                    ]
                ]
        Nothing -> div [] []

storeButton : Model -> ProjectModel -> Html Msg
storeButton model pm =
    storeView pm model.viewEditorModel |> buttonClick model (makeIndex viewEditorIdx "btnStr") "Store"

storeView : ProjectModel -> ViewEditorModel -> Msg
storeView pm vm =
    let
        viewEditRowToViewRow (ViewEditRow items) = List.map (\i -> i.content) items |> ViewRow
        storeValidViewToEdit v vn =
            Send (WRStoreView pm.project.projectName
                     { viewName = vn
                     , rows = List.map viewEditRowToViewRow v.rows
                     }
                 )
        storeViewWithName vn = case vm.viewToEdit of
            Just v -> storeValidViewToEdit v vn
            Nothing -> Internal (ShowMessage "Please fill the view")
    in
        case vm.viewName of
            Just vn -> storeViewWithName vn
            Nothing -> Internal (ShowMessage "Please enter a view name")


viewsList : Model -> Project -> Html Msg
viewsList model p =
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
                ((Lists.onSelectListItem sendLoadView) :: (scrollableListStyle 55))
                (List.map listItem p.viewNames)
        ]

viewEditorTable : Model -> Maybe ViewEdit -> Html Msg
viewEditorTable model mv =
    case mv of
        Just v -> DataTable.table [] [ viewEditRows model v ]
        Nothing -> DataTable.table [] [ DataTable.tbody [] [] ]

viewEditRows : Model -> ViewEdit -> Html Msg
viewEditRows model v =
    let
        rows =
            ListX.zip (List.range 0 (List.length v.rows)) v.rows |> List.map (viewEditRowToTableCells model) |> List.map (DataTable.tr [])
    in
        DataTable.tbody [] rows


viewChoice : Model -> Int -> Html Msg
viewChoice model rowI =
    DataTable.td []
        [ RadioButton.view Mdc
            (makeIndex viewEditorIdx "radChc")
            model.mdc
            [ RadioButton.selected |> Options.when (model.viewEditorModel.rowToAddTo == rowI)
            , Options.onClick (Internal (ChangeViewEditSelectedRow rowI))
            ]
            []
        ]

viewEditItem : Model -> ViewEditItem -> Html Msg
viewEditItem model item =
    case item.content of
        MapItem mn -> DataTable.td [ Options.css "background" "Coral"] [ viewEditItemCheckbox model item.id (xmapNameToString mn) ]
        LabelItem l -> DataTable.td [ Options.css "background" "DarkTurquoise"] [ viewEditItemCheckbox model item.id l ]


viewEditItemCheckbox: Model -> ViewEditItemId -> String -> Html Msg
viewEditItemCheckbox model viewItemId txt=
    let checked =
            Dict.get viewItemId model.viewEditorModel.checkedViewEditItems
                |> Maybe.withDefault Nothing
                |> Maybe.map Checkbox.checked
                |> Maybe.withDefault Options.nop
        clickHandler = Options.onClick (Internal (ChangeViewEditCheckedItem viewItemId))
        index = makeIndex viewEditorIdx ("check" ++ (String.fromInt viewItemId))
    in
        div []
        [
            Checkbox.view Mdc index model.mdc (checked :: clickHandler :: []) [],
            text txt
        ]

viewEditRowToTableCells : Model -> ( Int, ViewEditRow ) -> List (Html Msg)
viewEditRowToTableCells model ( rowIdx, ViewEditRow row ) =
    viewChoice model rowIdx :: List.map (viewEditItem model) row


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
                    ((Lists.onSelectListItem selectItem) :: (scrollableListStyle 37))
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
        div []
            [ TextField.view Mdc
                (makeIndex viewEditorIdx "txtAddLbl")
                model.mdc
                [ TextField.label "Label name"
                , TextField.value viewEditorModel.labelEditing
                , Options.onInput (\s -> Internal (UpdateViewLabel s)),
                Options.css "margin-top" "6px"
                ]
                []
            , buttonClick model (makeIndex viewEditorIdx "btnAddLbl") "Add label" newLabelMessage
            ]


addRowButton : Model -> Html Msg
addRowButton model =
    buttonClick model (makeIndex viewEditorIdx "btnAddRow") "Add row" (Internal AddRowToView)

removeCellsButton : Model -> Html Msg
removeCellsButton model =
    let
        itemsToDelete = Dict.toList model.viewEditorModel.checkedViewEditItems
            |> List.filter (\(k, v)-> Maybe.withDefault False v)
            |> List.map (\(k, v) -> k)
    in
        buttonClick model (makeIndex viewEditorIdx "btnRemoveCells") "Remove items" (Internal (RemoveItemsFromView itemsToDelete))
