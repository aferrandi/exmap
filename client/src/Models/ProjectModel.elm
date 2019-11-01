module Models.ProjectModel exposing (..)

import Types.Calculation exposing (..)
import Dict as Dict
import Models.InternalMessages exposing (..)
import List.Extra as ListX
import Material exposing (Msg)
import Types.Project exposing (..)
import Types.Views exposing (..)
import Models.WebMessages exposing (WebRequest)
import Types.XMapTypes exposing (..)


type alias XMapByName = Dict.Dict XMapName XMap

type alias OperationTypesByName =
    Dict.Dict (OperationCategory, OperationName) OperationType

type alias OperationIdByCategory =
    Dict.Dict OperationCategory (List OperationId)

type alias TableConfiguration =
    { columnsWidths : List Int
    }

type alias FunctionsModel =
    { idsByCategory : OperationIdByCategory
    , typesById : OperationTypesByName
    }

type alias ViewModel =
    { view : View
    , maps : XMapByName
    }

type alias ProjectModel =
    { project : Project
    , openViews : List ViewModel
    }

type alias XMapEditorModel =
    { xmapName : Maybe XMapName
    , xmapType : XMapType
    , xmapToEdit : Maybe XMap
    , xmapEditing : Maybe String
    , newXmapName : String
    , tableConf : TableConfiguration
    }

type alias ViewEditorModel =
    { viewName : Maybe ViewName
    , viewToEdit : Maybe View
    , newViewName : ViewName
    , labelEditing : String
    , rowToAddTo : Int
    , selectedViewCells: Dict.Dict String (Maybe Bool)
    }


type alias CalculationEditorModel =
    { calculationName : Maybe CalculationName
    , resultMapName : Maybe String
    , operationMode : OperationMode
    , calculationFormulaText : Maybe CalculationFormulaText
    , newCalculationName : CalculationName
    }


type alias Model =
    { mdc : Material.Model Msg
    , openProjects : List ProjectModel
    , allProjects : List ProjectName
    , currentProject : Maybe ProjectName
    , currentView : Maybe ViewName
    , newProjectName : String
    , messages : List Error
    , xmapEditorModel : XMapEditorModel
    , viewEditorModel : ViewEditorModel
    , calculationEditorModel : CalculationEditorModel
    , currentProjectForm : ProjectFormType
    , currentCategory : Maybe OperationCategory
    , mapsInProject : List XMapName
    , functions : Maybe FunctionsModel
    , openDialog : Maybe String
    }

type Msg
    = Receive String
    | Send WebRequest
    | SendMany (List WebRequest)
    | Mdc (Material.Msg Msg)
    | Internal InternalMsg
    | WebSocketConnected Bool
    | None

mdlIdxProjects = 0

mdlIdxViews = 1

openProjectWithName : Model -> ProjectName -> Maybe ProjectModel
openProjectWithName model pn =
    ListX.find (\pm -> pm.project.projectName == pn) model.openProjects

currentProjectModel : Model -> Maybe ProjectModel
currentProjectModel model =
    Maybe.andThen (openProjectWithName model) model.currentProject

openViewWithName : Model -> ViewName -> Maybe ViewModel
openViewWithName model vn =
    let
        findView ovs = ListX.find (\vm -> vm.view.viewName == vn) ovs
    in
        Maybe.andThen (\pm -> findView pm.openViews) (currentProjectModel model)
