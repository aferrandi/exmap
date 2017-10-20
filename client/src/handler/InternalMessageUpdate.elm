module InternalMessageUpdate exposing (updateInternal)

import ProjectModel exposing (..)

import XMapText exposing (..)
import XMapTypes exposing (..)
import XMapParse exposing (..)
import ModelUpdate exposing (..)
import MapsExtraction exposing (xmapNameToString)
import ServerMessaging exposing (..)
import WebMessages exposing (..)
import Calculation exposing (..)

updateInternal : InternalMsg -> Model -> (Model, Cmd Msg)
updateInternal msg model = case msg of
    SelectProjectTab idx -> ({ model | projectTab = idx }, Cmd.none)
    SelectViewTab idx -> ({ model | viewTab = idx }, Cmd.none)
    MapToTextArea-> (handleMapToTextArea model, Cmd.none)
    MapToTable-> ( handleMapToTable model, Cmd.none)
    TextToMapTextArea s -> ( updateXMapEditorModel model (\xm ->{ xm | xmapEditing = Just s }), Cmd.none)
    NewMapName  s -> ( updateXMapEditorModel model (\xm ->{ xm | newXmapName = s }), Cmd.none)
    ShowMessage s -> ( showMessage model s, Cmd.none)
    SwitchProjectViewTo vt -> handleSwitchProjectViewTo model vt
    TextToCalculationTextArea s -> ( updateCalculationEditorModel model (\cm ->{ cm | calculationFormulaText = Just s }), Cmd.none)
    TextToResultNameText mn -> handleTextToResultNameText model mn
    AddMapToCalculation mn -> ( appendToFormulaText model (xmapNameToString mn), Cmd.none)
    AddApplicationToCalculation an -> ( appendToFormulaText model (an ++ " p"), Cmd.none)
    AddOperationToCalculation on ->  ( appendToFormulaText model (on ++ " p1 p2"), Cmd.none)
    ChangeOperationMode om -> handleChangeOperationMode model om


handleChangeOperationMode : Model -> OperationMode -> (Model, Cmd Msg)
handleChangeOperationMode model om = ( updateCalculationEditorModel model (\cm ->{  cm | operationMode = om }), Cmd.none)

handleSwitchProjectViewTo : Model -> ProjectViewType -> (Model, Cmd Msg)
handleSwitchProjectViewTo  model vt =
    let functionRequest : Maybe (Cmd Msg)
        functionRequest = case model.calculationEditorModel.functions of
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
                             mm = Maybe.map2 textToMap xmapEditorModel.xmapType xmapEditorModel.xmapEditing
                         in case mm of
                                   Just (Ok m) -> updateXMapEditorModel model (\xm -> { xm | xmapToEdit = Just m })
                                   Just (Err e) -> showMessage model e
                                   Nothing -> model

handleMapToTextArea : Model -> Model
handleMapToTextArea model = updateXMapEditorModel model (\xm -> { xm | xmapEditing = Maybe.map mapToText xm.xmapToEdit })
