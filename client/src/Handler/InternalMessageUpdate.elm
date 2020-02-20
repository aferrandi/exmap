module Handler.InternalMessageUpdate exposing (updateInternal)

import Handler.InternalCalculationMessageUpdate exposing (appendToFormulaText, handleAddOperationToCalculation, handleChangeOperationMode, handleChangeOperationsMatch, handleNewCalculationWithName, handleSwitchCategoryTo, handleTextToResultNameText, handleUpdateCalculationName)
import Handler.InternalMapMessageUpdate exposing (handleChangeMapType, handleMapToTable, handleMapToTextArea, handleNewMapWithName, handleShowMapInEditor, handleUpdateMapName)
import Handler.InternalProjectMessageUpdate exposing (handleNewProjectWithName, handleOpenProject, handleSwitchProjectViewTo, handleUpdateProjectName)
import Handler.InternalVIewMessageUpdate exposing (handleAddItemToViewEdit, handleSelectMapIndexForViewEdit, handleAddRowToViewEdit, handleChangeViewEditCheckedItem, handleChangeViewEditSelectedRow, handleNewViewWithName, handleOpenView, handleRemoveItemsFromViewEdit, handleUpdateViewLabel, handleUpdateViewName)
import Models.InternalMessages exposing (..)
import Transform.MapsExtraction exposing (xmapNameToString)
import Maybe
import Handler.ModelUpdate exposing (..)
import Models.ProjectModel exposing (..)


updateInternal : InternalMsg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        MapToTextArea ->
            ( handleMapToTextArea model, Cmd.none )
        MapToTable ->
            ( handleMapToTable model, Cmd.none )
        OpenProject pn ->
            handleOpenProject model pn
        OpenView vn ->
            handleOpenView model vn
        TextToMapTextArea s ->
            ( updateXMapEditorModel model (\xm -> { xm | xmapEditing = Just s }), Cmd.none )
        UpdateMapName s ->
            ( handleUpdateMapName model s, Cmd.none )
        UpdateCalculationName s ->
            ( handleUpdateCalculationName model s, Cmd.none )
        UpdateViewName s ->
            ( handleUpdateViewName model s, Cmd.none )
        UpdateProjectName s ->
            ( handleUpdateProjectName model s, Cmd.none )
        UpdateViewLabel s ->
            ( handleUpdateViewLabel model s, Cmd.none )
        NewCalculationWithName cn ->
            ( handleNewCalculationWithName model cn, Cmd.none )
        NewViewWithName vn ->
            ( handleNewViewWithName model vn, Cmd.none )
        NewMapWithName mn mt ->
            ( handleNewMapWithName model mn mt, Cmd.none )
        NewProjectWithName pn ->
            handleNewProjectWithName model pn
        ShowMessage s ->
            ( showMessage model s, Cmd.none )
        ShowMapInEditor mn ->
            handleShowMapInEditor model mn
        SwitchProjectViewTo vt ->
            handleSwitchProjectViewTo model vt
        SwitchCategoryTo ct ->
            ( handleSwitchCategoryTo model ct, Cmd.none )
        TextToCalculationTextArea s ->
            ( updateCalculationEditorModel model (\cm -> { cm | calculationFormulaText = Just s }), Cmd.none )
        TextToResultNameText mn ->
            ( handleTextToResultNameText model mn, Cmd.none )
        AddMapToCalculation mn ->
            ( appendToFormulaText model (xmapNameToString mn), Cmd.none )
        AddOperationToCalculation on ->
            ( handleAddOperationToCalculation model on, Cmd.none )
        ChangeOperationMode om ->
            ( handleChangeOperationMode model om, Cmd.none )
        ChangeOperationsMatch om ->
            ( handleChangeOperationsMatch model om, Cmd.none )
        ChangeMapType mt ->
            ( handleChangeMapType model mt, Cmd.none )
        AddItemToView row it ->
            ( handleAddItemToViewEdit model row it, Cmd.none )
        SelectMapIndexForView idx ->
            ( handleSelectMapIndexForViewEdit model idx, Cmd.none )
        AddRowToView ->
            ( handleAddRowToViewEdit model, Cmd.none )
        RemoveItemsFromView viewEditItemIds ->
            ( handleRemoveItemsFromViewEdit model viewEditItemIds, Cmd.none )
        ChangeViewEditSelectedRow row ->
            ( handleChangeViewEditSelectedRow model row, Cmd.none )
        ChangeViewEditCheckedItem viewEditItemId ->
            ( handleChangeViewEditCheckedItem model viewEditItemId, Cmd.none )
        ShowDialog index ->
             ( showDialog model index, Cmd.none)
        CloseDialog ->
             ( closeDialog model, Cmd.none)
        CloseDialogWithError err ->
            ( showMessage model err |> closeDialog, Cmd.none )
