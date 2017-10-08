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
    NewMap -> (model, Cmd.none)
    NewView -> (model, Cmd.none)
    MapToTextArea-> (handleMapToTextArea model, Cmd.none)
    MapToTable-> ( handleMapToTable model, Cmd.none)
    TextToTextArea s -> ( { model | xmapEditing = Just s }, Cmd.none)

handleMapToTable : Model -> Model
handleMapToTable model = let mm = Maybe.map2 textToMap model.xmapType model.xmapEditing
                         in case mm of
                                   Just (Ok m) -> { model | xmapToEdit = Just m }
                                   Just (Err e) -> showMessage model e
                                   Nothing -> model

handleMapToTextArea : Model -> Model
handleMapToTextArea model = { model | xmapEditing = Maybe.map mapToText model.xmapToEdit }
