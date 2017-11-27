module InternalMessageUpdate exposing (updateInternal)

import List.Extra as ListX
import Maybe as Maybe

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
import Project exposing (..)


updateInternal : InternalMsg -> Model -> (Model, Cmd Msg)
updateInternal msg model = case msg of
    MapToTextArea-> (handleMapToTextArea model, Cmd.none)
    MapToTable-> ( handleMapToTable model, Cmd.none)
    OpenProject pn -> handleOpenProject model pn
    OpenView vn -> handleOpenView model vn
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
    ShowMapInEditor mn -> handleShowMapInEditor model mn
    SwitchProjectViewTo vt -> handleSwitchProjectViewTo model vt
    TextToCalculationTextArea s -> ( updateCalculationEditorModel model (\cm ->{ cm | calculationFormulaText = Just s }), Cmd.none)
    TextToResultNameText mn -> (handleTextToResultNameText model mn, Cmd.none)
    AddMapToCalculation mn -> ( appendToFormulaText model (xmapNameToString mn), Cmd.none)
    AddApplicationToCalculation an -> ( appendToFormulaText model (an ++ " p"), Cmd.none)
    AddOperationToCalculation on ->  ( appendToFormulaText model (on ++ " p1 p2"), Cmd.none)
    ChangeOperationMode om -> (handleChangeOperationMode model om, Cmd.none)
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

handleOpenProject : Model -> ProjectName -> (Model, Cmd Msg)
handleOpenProject model pn =
    let command = case openProjectWithName model pn of
                    Just pm -> Cmd.none
                    Nothing -> sendToServer (WRSubscribeToProject pn)
    in ({ model | currentProject = Just pn }, command)

handleOpenView : Model -> ViewName -> (Model, Cmd Msg)
handleOpenView model vn =
    let command = case openViewWithName model vn of
                    Just v -> Cmd.none
                    Nothing -> Maybe.withDefault Cmd.none (Maybe.map (\pn -> sendToServer (WRSubscribeToView pn vn)) model.currentProject)
    in ({ model | currentView = Just vn }, command)


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

handleChangeOperationMode : Model -> OperationMode -> Model
handleChangeOperationMode model om = updateCalculationEditorModel model (\cm ->{  cm | operationMode = om })

handleSwitchProjectViewTo : Model -> ProjectFormType -> (Model, Cmd Msg)
handleSwitchProjectViewTo  model vt =
    let functionRequest : Maybe (Cmd Msg)
        functionRequest = case model.functions of
                                Just fs -> Nothing
                                Nothing -> Just (sendToServer WRFunctions)
        mapsInProjectRequest = Maybe.map (\pm -> sendToServer (WRMapsInProject pm.project.projectName)) (currentProjectModel model)
    in { model | currentProjectForm = vt } ! List.filterMap identity [functionRequest, mapsInProjectRequest]

handleTextToResultNameText : Model -> String -> Model
handleTextToResultNameText model mn = updateCalculationEditorModel model (\cm ->{  cm | resultMapName = Just mn })

handleShowMapInEditor : Model -> XMapName -> (Model, Cmd Msg)
handleShowMapInEditor model mn =
    let cleanup =  updateXMapEditorModel model (\mm -> { mm | xmapEditing = Nothing })
        command pn = sendToServer (WRLoadMap pn mn)
    in case model.currentProject of
        Just pn -> (cleanup, command pn)
        Nothing -> (model, Cmd.none)

handleMapToTable : Model -> Model
handleMapToTable model = let xmapEditorModel = model.xmapEditorModel
                             mm = Maybe.map (textToMap xmapEditorModel.xmapType) xmapEditorModel.xmapEditing
                         in case mm of
                                   Just (Ok m) -> updateXMapEditorModel model (\xm -> { xm | xmapToEdit = Just m, xmapEditing = Nothing })
                                   Just (Err e) -> showMessage model e
                                   Nothing -> model

appendToFormulaText : Model -> String -> Model
appendToFormulaText model s =
    let updateFormulaText cm = (Maybe.withDefault "" cm.calculationFormulaText) ++ " " ++ s
    in updateCalculationEditorModel model (\cm -> {cm | calculationFormulaText = Just (updateFormulaText cm)})


handleMapToTextArea : Model -> Model
handleMapToTextArea model = updateXMapEditorModel model (\xm -> { xm | xmapEditing = Maybe.map mapToText xm.xmapToEdit })

emptyRow : ViewRow
emptyRow = ViewRow []

