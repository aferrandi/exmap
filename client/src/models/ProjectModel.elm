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
    , xmapType : Maybe XMapType
    , xmapToEdit : Maybe XMap
    , xmapEditing : Maybe String
    , newXmapName : String
    }

type alias ViewEditorModel = {
    viewName : Maybe ViewName
    , viewToEdit : Maybe View
    , newViewName : ViewName
    }

type alias CalculationEditorModel = {
    calculationName : Maybe CalculationName
    , resultMapName : Maybe String
    , operationMode : OperationMode
    , calculationFormulaText : Maybe CalculationFormulaText
    , newCalculationName : CalculationName
    }



type alias Model = {
    openProjects : List ProjectModel
    , allProjects : List ProjectName
    , newProjectName : String
    , mdl : Material.Model
    , projectTab : Int
    , viewTab : Int
    , messages : List Error
    , xmapEditorModel : XMapEditorModel
    , viewEditorModel : ViewEditorModel
    , calculationEditorModel : CalculationEditorModel
    , currentProjectView : ProjectViewType
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

currentOpenProject : Model -> Maybe ProjectModel
currentOpenProject model = ListX.getAt model.projectTab model.openProjects