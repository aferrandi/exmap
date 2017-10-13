module ModelUpdate exposing (..)

import ProjectModel exposing (..)

showMessage : Model -> String -> Model
showMessage model msg = { model | messages = msg :: model.messages }

updateXMapEditorModel : Model -> (XMapEditorModel -> XMapEditorModel) -> Model
updateXMapEditorModel model update = { model | xmapEditorModel = update model.xmapEditorModel }

updateViewEditorModel : Model -> (ViewEditorModel -> ViewEditorModel) -> Model
updateViewEditorModel model update = { model | viewEditorModel = update model.viewEditorModel }