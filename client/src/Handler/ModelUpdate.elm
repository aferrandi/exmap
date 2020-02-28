module Handler.ModelUpdate exposing (..)

import Display.MdcIndexes exposing (makeIndex, projectsUIIdx)
import Material.Snackbar as Snackbar
import Models.InternalMessages exposing (InternalMsg(..))
import Models.ProjectModel exposing (..)


showMessage : Model -> String -> ( Model, Cmd Msg )
showMessage model msg =
    let
        ( updatedModel, effects ) = showMessageAsSnackbar model msg
    in
        ({ updatedModel | messages = msg :: model.messages }, effects)

showMessageAsSnackbar : Model -> String ->  ( Model, Cmd Msg )
showMessageAsSnackbar model msg =
    let
        idxDialog = makeIndex projectsUIIdx "dlgMsg"
        command = Just (Internal (ShowDialog idxDialog))
        content = Snackbar.snack command msg  "Show all"
        ( mdc, effects ) = Snackbar.add Mdc "snackMessage" content model.mdc
    in
         ( { model | mdc = mdc }, effects )

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
