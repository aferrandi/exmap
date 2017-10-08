module ModelUpdate exposing (..)

import ProjectModel exposing (..)

showMessage : Model -> String -> Model
showMessage model msg = { model | messages = msg :: model.messages }