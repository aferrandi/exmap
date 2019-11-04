module Handler.InternalMapMessageUpdate exposing (..)

import Handler.ModelUpdate exposing (showMessage, updateXMapEditorModel)
import Models.EmptyModel exposing (emptyXMapEditorModel)
import Models.ProjectModel exposing (Model, Msg, closeDialog)
import Models.WebMessages exposing (WebRequest(..))
import Server.ServerMessaging exposing (sendToServer)
import Transform.XMapParse exposing (textToMap)
import Transform.XMapText exposing (mapToText)
import Types.XMapTypes exposing (XMapName, XMapType)

handleUpdateMapName : Model -> String -> Model
handleUpdateMapName model s =
    updateXMapEditorModel model (\xm -> { xm | newXmapName = s })

handleNewMapWithName : Model -> XMapName -> XMapType -> Model
handleNewMapWithName model mn mt =
    closeDialog { model | xmapEditorModel = { emptyXMapEditorModel | xmapName = Just mn, xmapType = mt } }

handleChangeMapType : Model -> XMapType -> Model
handleChangeMapType model mt =
    updateXMapEditorModel model (\xm -> { xm | xmapType = mt })

handleShowMapInEditor : Model -> XMapName -> ( Model, Cmd Msg )
handleShowMapInEditor model mn =
    let
        cleanup = updateXMapEditorModel model (\mm -> { mm | xmapEditing = Nothing })
        command pn = sendToServer (WRLoadMap pn mn)
    in
        case model.currentProject of
            Just pn -> ( cleanup, command pn )
            Nothing -> ( model, Cmd.none )

handleMapToTable : Model -> Model
handleMapToTable model =
    let
        xmapEditorModel = model.xmapEditorModel
        mm = Maybe.map (textToMap xmapEditorModel.xmapType) xmapEditorModel.xmapEditing
    in
        case mm of
            Just (Ok m) -> updateXMapEditorModel model (\xm -> { xm | xmapToEdit = Just m, xmapEditing = Nothing })
            Just (Err e) -> showMessage model e
            Nothing -> model


handleMapToTextArea : Model -> Model
handleMapToTextArea model =
    updateXMapEditorModel model (\xm -> { xm | xmapEditing = Maybe.map mapToText xm.xmapToEdit })
