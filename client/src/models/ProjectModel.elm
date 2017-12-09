module ProjectModel exposing (..)

import Material
import Html exposing (Html)
import Dict as Dict

import Project exposing (..)
import Views exposing (..)
import XMapTypes exposing (..)
import Calculation exposing (..)
import WebMessages exposing (WebRequest)
import InternalMessages exposing (..)
import List.Extra as ListX

type alias Mdl = Material.Model

type alias XMapByName = Dict.Dict XMapName XMap

type alias TableConfiguration = {
    columnsWidths : List Int
    }

type alias ViewModel = {
    view : View
    , maps : XMapByName
    }

type alias ProjectModel = {
    project : Project
    , openViews : List ViewModel
    }

type alias XMapEditorModel = {
    xmapName : Maybe XMapName
    , xmapType : XMapType
    , xmapToEdit : Maybe XMap
    , xmapEditing : Maybe String
    , newXmapName : String
    , tableConf : TableConfiguration
    }

type alias ViewEditorModel = {
    viewName : Maybe ViewName
    , viewToEdit : Maybe View
    , newViewName : ViewName
    , labelEditing : String
    , rowToAddTo : Int
    }

type alias CalculationEditorModel = {
    calculationName : Maybe CalculationName
    , resultMapName : Maybe String
    , operationMode : OperationMode
    , calculationFormulaText : Maybe CalculationFormulaText
    , newCalculationName : CalculationName
    }

type alias Model = {
    mdl : Material.Model
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
    , mapsInProject : List XMapName
    , functions : Maybe Functions
    }

type Msg
  = Receive String
  | Send WebRequest
  | SendMany (List WebRequest)
  | Mdl (Material.Msg Msg)
  | Internal InternalMsg

mdlIdxProjects = 0
mdlIdxViews = 1

openProjectWithName : Model -> ProjectName -> Maybe ProjectModel
openProjectWithName model pn = ListX.find (\pm -> pm.project.projectName == pn) model.openProjects

currentProjectModel : Model -> Maybe ProjectModel
currentProjectModel model = Maybe.andThen (openProjectWithName model) model.currentProject

openViewWithName : Model -> ViewName -> Maybe ViewModel
openViewWithName model vn =
    let findView ovs = ListX.find (\vm -> vm.view.viewName == vn) ovs
    in Maybe.andThen (\pm -> findView pm.openViews) (currentProjectModel model)