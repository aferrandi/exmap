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
type alias ViewEditItemsChecked = Dict.Dict ViewEditItemId (Maybe Bool)


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
    , isNew : Bool
    }

type alias ViewEditorModel =
    { viewName : Maybe ViewName
    , viewToEdit : Maybe ViewEdit
    , newViewName : ViewName
    , selectedMapIdx : Maybe Int
    , labelEditing : String
    , rowToAddTo : Int
    , lastViewEditItemId : ViewEditItemId
    , checkedViewEditItems: ViewEditItemsChecked
    , isNew : Bool
    }


type alias CalculationEditorModel =
    { calculationName : Maybe CalculationName
    , resultMapName : Maybe String
    , operationMode : OperationMode
    , calculationFormulaText : Maybe CalculationFormulaText
    , newCalculationName : CalculationName
    , operationsMatch : String
    , selectedMapIdx : Maybe Int
    , selectedFunctionIdx : Maybe Int
    , isNew : Bool
    }

type alias Heights =
    { viewProjects : Int
    , viewAllProjectsList : Int
    , viewMessages : Int
    , viewViews: Int
    , viewAllViewsList : Int
    , viewCalculationsEditor : Int
    , viewEditorForCalculation : Int
    , calculationTextArea : Int
    , calculationsInProjectList : Int
    , mapsInProjectListForCalculation : Int
    , functionsNamesList : Int
    , mapEditorView : Int
    , mapEditorViewForMap : Int
    , mapEditorTextArea : Int
    , mapEditorTableFull : Int
    , mapEditorMapList : Int
    , viewViewsEditor : Int
    , viewEditorForView : Int
    , viewEditorViewsList : Int
    , viewEditorMapList : Int
    , viewEditRows : Int
    , viewView: Int
    , newMapDialog : Int
    }

type alias UI =
    {
    heights : Heights
    }


type alias Model =
    { mdc : Material.Model Msg
    , ui: UI
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

closeDialog : Model ->  Model
closeDialog model = { model | openDialog = Nothing }

showDialog : Model -> String -> Model
showDialog model index = { model | openDialog = Just index }

newViewEditItemId: ViewEditorModel -> (ViewEditItemId, ViewEditorModel)
newViewEditItemId vm =
    let
        updateModel = { vm | lastViewEditItemId = vm.lastViewEditItemId + 1}
    in
        (updateModel.lastViewEditItemId, updateModel)