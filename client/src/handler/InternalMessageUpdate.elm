module InternalMessageUpdate exposing (updateInternal)

import ProjectModel exposing (..)

import XMapText exposing (..)
import XMapTypes exposing (..)
import XMapParse exposing (..)
import ModelUpdate exposing (..)
import MapsExtraction exposing (xmapNameToString)

updateInternal : InternalMsg -> Model -> (Model, Cmd Msg)
updateInternal msg model = case msg of
    SelectProjectTab idx -> ({ model | projectTab = idx }, Cmd.none)
    SelectViewTab idx -> ({ model | viewTab = idx }, Cmd.none)
    MapToTextArea-> (handleMapToTextArea model, Cmd.none)
    MapToTable-> ( handleMapToTable model, Cmd.none)
    TextToTextArea s -> ( updateXMapEditorModel model (\xm ->{ xm | xmapEditing = Just s }), Cmd.none)
    NewMapName  s -> ( updateXMapEditorModel model (\xm ->{ xm | newXmapName = s }), Cmd.none)
    ShowMessage s -> ( showMessage model s, Cmd.none)
    SwitchProjectViewTo vt -> ({ model | currentProjectView = vt }, Cmd.none)
    AddMapToCalculation mn -> ( handleAddMapToCalculation model mn , Cmd.none)


handleAddMapToCalculation : Model -> XMapName -> Model
handleAddMapToCalculation model mn =
    let updateFormulaText cm = (Maybe.withDefault "" cm.calculationFormulaText) ++ " " ++ (xmapNameToString mn)
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
