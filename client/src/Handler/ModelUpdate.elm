module Handler.ModelUpdate exposing (..)

import Models.ProjectModel exposing (..)


showMessage : Model -> String -> Model
showMessage model msg =
    { model | messages = msg :: model.messages }


updateXMapEditorModel : Model -> (XMapEditorModel -> XMapEditorModel) -> Model
updateXMapEditorModel model update =
    { model | xmapEditorModel = update model.xmapEditorModel }


updateViewEditorModel : Model -> (ViewEditorModel -> ViewEditorModel) -> Model
updateViewEditorModel model update =
    { model | viewEditorModel = update model.viewEditorModel }


updateCalculationEditorModel : Model -> (CalculationEditorModel -> CalculationEditorModel) -> Model
updateCalculationEditorModel model update =
    { model | calculationEditorModel = update model.calculationEditorModel }


updateOpenProjects : Model -> (List ProjectModel -> List ProjectModel) -> Model
updateOpenProjects model update =
    { model | openProjects = update model.openProjects }
