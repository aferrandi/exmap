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
    let
        title =
            case model.viewEditorModel.viewName of
                Just viewName -> "Editing view: " ++ viewName
                Nothing -> "View Editor"
    in
        div []
            [ titleWithIcon title "view_module" "Pink"
            , LayoutGrid.view [ heightInView 70 ]
                [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span2Desktop, LayoutGrid.span1Phone] [ viewsList model pm.project, newViewButton model ]
                , LayoutGrid.cell [LayoutGrid.span6Tablet, LayoutGrid.span10Desktop, LayoutGrid.span3Phone] [ viewEditorForView model pm ]
                ]
            ]

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
                    [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span4Desktop, LayoutGrid.span4Phone] []
                    , LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span4Desktop, LayoutGrid.span2Phone] [ addRowButton model ]
                    , LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span4Desktop, LayoutGrid.span2Phone] [ storeButton model pm ]
                    ]
                ]
        Nothing -> div [] []

storeButton : Model -> ProjectModel -> Html Msg
storeButton model pm =
    storeView pm model.viewEditorModel |> buttonClick model (makeIndex viewEditorIdx "btnStr") "Store"

storeView : ProjectModel -> ViewEditorModel -> Msg
storeView pm vm =
    case vm.viewName of
        Just vn ->
            case vm.viewToEdit of
                Just v ->Send (WRStoreView pm.project.projectName
                                { viewName = vn
                                , rows = v.rows
                                }
                            )
                Nothing -> Internal (ShowMessage "Please fill the view")
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
        Lists.ul Mdc (makeIndex viewEditorIdx "lstVew") model.mdc
            ((Lists.onSelectListItem sendLoadView) :: (scrollableListStyle 55))
            (List.map listItem p.viewNames)


viewEditorTable : Model -> Maybe View -> Html Msg
viewEditorTable model mv =
    case mv of
        Just v -> DataTable.table [] [ viewRows model v ]
        Nothing -> DataTable.table [] [ DataTable.tbody [] [] ]

viewRows : Model -> View -> Html Msg
viewRows model v =
    let
        rows =
            ListX.zip (List.range 0 (List.length v.rows)) v.rows |> List.map (viewRowToTableCells model) |> List.map (DataTable.tr [])
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

viewCell : Model -> ViewItem -> Html Msg
viewCell model item =
    case item of
        MapItem mn -> DataTable.td [ Options.css "background" "Coral"] [ viewCellCheckbox model (xmapNameToString mn) ]
        LabelItem l -> DataTable.td [ Options.css "background" "DarkTurquoise"] [ viewCellCheckbox model l ]


viewCellCheckbox: Model -> String -> Html Msg
viewCellCheckbox model txt =
    let checked =
            Dict.get index model.viewEditorModel.selectedViewCells
                |> Maybe.withDefault Nothing
                |> Maybe.map Checkbox.checked
                |> Maybe.withDefault Options.nop
        clickHandler = Options.onClick (Internal (ChangeViewEditCheckedItem index))
        index = makeIndex viewEditorIdx txt
    in
        div []
        [
            Checkbox.view Mdc index model.mdc (checked :: clickHandler :: []) [],
            text txt
        ]

viewRowToTableCells : Model -> ( Int, ViewRow ) -> List (Html Msg)
viewRowToTableCells model ( rowIdx, ViewRow row ) =
    viewChoice model rowIdx :: List.map (viewCell model) row


viewEditorMapList : Model -> Html Msg
viewEditorMapList model =
    let
        buildMsg mn = (Internal (AddItemToView model.viewEditorModel.rowToAddTo (MapItem mn)))
        sendAddItem index = sendListMsg buildMsg model.mapsInProject index
        listItem mn =
            Lists.li []
                [
                    Lists.graphicIcon []"list" ,
                    text (xmapNameToString mn)
                ]
    in
        Lists.ul Mdc (makeIndex viewEditorIdx "lstMap") model.mdc
            ((Lists.onSelectListItem sendAddItem) :: (scrollableListStyle 40))
            (List.map listItem model.mapsInProject)

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
                , Options.onInput (\s -> Internal (UpdateViewLabel s))
                ]
                []
            , buttonClick model (makeIndex viewEditorIdx "btnAddLbl") "Add label" newLabelMessage
            ]

addRowButton : Model -> Html Msg
addRowButton model =
    buttonClick model (makeIndex viewEditorIdx "btnAddRow") "Add row" (Internal AddRowToView)
