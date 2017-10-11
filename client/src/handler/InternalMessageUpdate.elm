module InternalMessageUpdate exposing (updateInternal)

import ProjectModel exposing (..)

import XMapText exposing (..)
import XMapParse exposing (..)
import ModelUpdate exposing (..)

updateInternal : InternalMsg -> Model -> (Model, Cmd Msg)
updateInternal msg model = case msg of
    SelectProjectTab idx -> ({ model | projectTab = idx }, Cmd.none)
    SelectViewTab idx -> ({ model | viewTab = idx }, Cmd.none)
    NewProject -> (model, Cmd.none)
    MapToTextArea-> (handleMapToTextArea model, Cmd.none)
    MapToTable-> ( handleMapToTable model, Cmd.none)
    TextToTextArea s -> ( updateXMapEditorModel model (\xm ->{ xm | xmapEditing = Just s }), Cmd.none)
    NewMapName  s -> ( updateXMapEditorModel model (\xm ->{ xm | newXmapName = s }), Cmd.none)
    ShowMessage s -> ( showMessage model s, Cmd.none)

handleMapToTable : Model -> Model
handleMapToTable model = let xmapEditorModel = model.xmapEditorModel
                             mm = Maybe.map2 textToMap xmapEditorModel.xmapType xmapEditorModel.xmapEditing
                         in case mm of
                                   Just (Ok m) -> updateXMapEditorModel model (\xm -> { xm | xmapToEdit = Just m })
                                   Just (Err e) -> showMessage model e
                                   Nothing -> model

handleMapToTextArea : Model -> Model
handleMapToTextArea model = updateXMapEditorModel model (\xm -> { xm | xmapEditing = Maybe.map mapToText xm.xmapToEdit })
