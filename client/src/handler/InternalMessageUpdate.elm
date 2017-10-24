module InternalMessageUpdate exposing (updateInternal)

import List.Extra as ListX

import ProjectModel exposing (..)

import XMapText exposing (..)
import XMapTypes exposing (..)
import XMapParse exposing (..)
import Views exposing (..)
import ModelUpdate exposing (..)
import MapsExtraction exposing (xmapNameToString)
import ServerMessaging exposing (..)
import WebMessages exposing (..)
import InternalMessages exposing (..)
import Calculation exposing (..)
import EmptyModel exposing (..)


updateInternal : InternalMsg -> Model -> (Model, Cmd Msg)
updateInternal msg model = case msg of
    SelectProjectTab idx -> ({ model | projectTab = idx }, Cmd.none)
    SelectViewTab idx -> ({ model | viewTab = idx }, Cmd.none)
    MapToTextArea-> (handleMapToTextArea model, Cmd.none)
    MapToTable-> ( handleMapToTable model, Cmd.none)
    TextToMapTextArea s -> ( updateXMapEditorModel model (\xm ->{ xm | xmapEditing = Just s }), Cmd.none)
    UpdateMapName  s -> ( handleUpdateMapName model s, Cmd.none)
    UpdateCalculationName  s -> ( handleUpdateCalculationName model s, Cmd.none)
    UpdateViewName  s -> ( handleUpdateViewName model s, Cmd.none)
    UpdateProjectName  s -> ( handleUpdateProjectName model s, Cmd.none)
    UpdateViewLabel s  -> ( handleUpdateViewLabel model s, Cmd.none)
    NewCalculationWithName cn -> ( handleNewCalculationWithName model cn , Cmd.none)
    NewViewWithName vn -> ( handleNewViewWithName model vn , Cmd.none)
    NewMapWithName mn mt  -> ( handleNewMapWithName model mn mt , Cmd.none)
    ShowMessage s -> ( showMessage model s, Cmd.none)
    SwitchProjectViewTo vt -> handleSwitchProjectViewTo model vt
    TextToCalculationTextArea s -> ( updateCalculationEditorModel model (\cm ->{ cm | calculationFormulaText = Just s }), Cmd.none)
    TextToResultNameText mn -> handleTextToResultNameText model mn
    AddMapToCalculation mn -> ( appendToFormulaText model (xmapNameToString mn), Cmd.none)
    AddApplicationToCalculation an -> ( appendToFormulaText model (an ++ " p"), Cmd.none)
    AddOperationToCalculation on ->  ( appendToFormulaText model (on ++ " p1 p2"), Cmd.none)
    ChangeOperationMode om -> handleChangeOperationMode model om
    ChangeMapType mt -> (handleChangeMapType model mt, Cmd.none)
    AddItemToView row it -> (handleAddItemToView model row it, Cmd.none)
    AddRowToView -> (handleAddRowToView model, Cmd.none)
    ChangeViewEditSelectedRow row -> ( handleChangeViewEditSelectedRow model row, Cmd.none)


handleChangeViewEditSelectedRow : Model -> Int -> Model
handleChangeViewEditSelectedRow model ri = updateViewEditorModel model (\vm -> { vm | rowToAddTo = ri })

handleUpdateCalculationName : Model -> String -> Model
handleUpdateCalculationName model s = updateCalculationEditorModel model (\cm ->{ cm | newCalculationName = s })

handleUpdateViewName : Model -> String -> Model
handleUpdateViewName model s = updateViewEditorModel model (\vm -> { vm | newViewName = s })

handleUpdateMapName : Model -> String -> Model
handleUpdateMapName model s = updateXMapEditorModel model (\xm -> { xm | newXmapName = s })

handleUpdateProjectName : Model -> String -> Model
handleUpdateProjectName model s = { model | newProjectName = s }

handleUpdateViewLabel : Model -> String -> Model
handleUpdateViewLabel model s = updateViewEditorModel model (\vm ->{ vm | labelEditing = s })

handleNewCalculationWithName : Model -> CalculationName -> Model
handleNewCalculationWithName model cn = { model | calculationEditorModel = { emptyCalculationEditorModel | calculationName = Just cn }}

emptyRow : ViewRow
emptyRow = ViewRow []

handleNewViewWithName : Model -> CalculationName -> Model
handleNewViewWithName model vn = { model | viewEditorModel = { emptyViewEditorModel | viewName = Just vn, viewToEdit = Just { viewName = vn, rows = [emptyRow]} }}

handleNewMapWithName : Model -> XMapName -> XMapType -> Model
handleNewMapWithName model mn mt = { model | xmapEditorModel = { emptyXMapEditorModel | xmapName = Just mn, xmapType = mt }}

handleChangeMapType : Model -> XMapType -> Model
handleChangeMapType model mt = updateXMapEditorModel model (\xm ->{ xm | xmapType = mt })

handleAddRowToView: Model -> Model
handleAddRowToView model =
    let updateView mv = case mv of
                        Just v ->  { v | rows = v.rows ++ [emptyRow]}
                        Nothing -> {viewName = "", rows = [emptyRow] }
    in updateViewEditorModel model (\vm -> {vm | viewToEdit = Just (updateView vm.viewToEdit) })

handleAddItemToView : Model -> Int -> ViewItem -> Model
handleAddItemToView model ri it =
    let updateRow (ViewRow r) = List.append r [it] |> ViewRow
        updateRows rs = ListX.updateAt ri updateRow rs |> Maybe.withDefault rs
        updateView mv = Maybe.map (\v -> {v | rows = updateRows v.rows}) mv
    in updateViewEditorModel model (\vm -> {vm | viewToEdit = updateView vm.viewToEdit })

handleChangeOperationMode : Model -> OperationMode -> (Model, Cmd Msg)
handleChangeOperationMode model om = ( updateCalculationEditorModel model (\cm ->{  cm | operationMode = om }), Cmd.none)

handleSwitchProjectViewTo : Model -> ProjectViewType -> (Model, Cmd Msg)
handleSwitchProjectViewTo  model vt =
    let functionRequest : Maybe (Cmd Msg)
        functionRequest = case model.functions of
                                Just fs -> Nothing
                                Nothing -> Just (sendToServer WRFunctions)
        mapsInProjectRequest = Maybe.map (\pm -> sendToServer (WRMapsInProject pm.project.projectName)) (currentOpenProject model)
    in { model | currentProjectView = vt } ! List.filterMap identity [functionRequest, mapsInProjectRequest]

handleTextToResultNameText : Model -> String -> (Model, Cmd Msg)
handleTextToResultNameText model mn = ( updateCalculationEditorModel model (\cm ->{  cm | resultMapName = Just mn }), Cmd.none)

appendToFormulaText : Model -> String -> Model
appendToFormulaText model s =
    let updateFormulaText cm = (Maybe.withDefault "" cm.calculationFormulaText) ++ " " ++ s
    in updateCalculationEditorModel model (\cm -> {cm | calculationFormulaText = Just (updateFormulaText cm)})

handleMapToTable : Model -> Model
handleMapToTable model = let xmapEditorModel = model.xmapEditorModel
                             mm = Maybe.map (textToMap xmapEditorModel.xmapType) xmapEditorModel.xmapEditing
                         in case mm of
                                   Just (Ok m) -> updateXMapEditorModel model (\xm -> { xm | xmapToEdit = Just m })
                                   Just (Err e) -> showMessage model e
                                   Nothing -> model

handleMapToTextArea : Model -> Model
handleMapToTextArea model = updateXMapEditorModel model (\xm -> { xm | xmapEditing = Maybe.map mapToText xm.xmapToEdit })
