module Handler.InternalProjectMessageUpdate exposing (..)

import Models.EmptyModel exposing (emptyCalculationEditorModel, emptyViewEditorModel, emptyXMapEditorModel)
import Models.InternalMessages exposing (ProjectFormType)
import Models.ProjectModel exposing (Model, Msg, closeDialog, currentProjectModel, openProjectWithName)
import Models.WebMessages exposing (WebRequest(..))
import Server.ServerMessaging exposing (sendToServer)
import Types.Project exposing (ProjectName)

handleUpdateProjectName : Model -> String -> Model
handleUpdateProjectName model s =
    { model | newProjectName = s }


handleOpenProject : Model -> ProjectName -> ( Model, Cmd Msg )
handleOpenProject model pn =
    let
        command =
            case openProjectWithName model pn of
                Just pm -> sendToServer (WRMapsInProject pn)
                Nothing -> sendToServer (WRSubscribeToProject pn)
    in
        ( { model
            | currentProject = Just pn
            , currentView = Nothing
            , xmapEditorModel = emptyXMapEditorModel
            , viewEditorModel = emptyViewEditorModel
            , calculationEditorModel = emptyCalculationEditorModel
          }
        , command
        )

handleNewProjectWithName : Model -> ProjectName -> ( Model, Cmd Msg )
handleNewProjectWithName model pn =
    ( closeDialog model, sendToServer (WRNewProject
                                              { projectName = model.newProjectName
                                              , calculations = []
                                              , viewNames = []
                                              , sources = []
                                              }
                                          ))

handleSwitchProjectViewTo : Model -> ProjectFormType -> ( Model, Cmd Msg )
handleSwitchProjectViewTo model vt =
    let
        functionRequest : Maybe (Cmd Msg)
        functionRequest =
            case model.functions of
                Just fs -> Nothing
                Nothing -> Just (sendToServer WRFunctions)
        mapsInProjectRequest =
            Maybe.map (\pm -> sendToServer (WRMapsInProject pm.project.projectName)) (currentProjectModel model)
    in
    ( { model | currentProjectForm = vt }
    , Cmd.batch (List.filterMap identity [ functionRequest, mapsInProjectRequest ])
    )
