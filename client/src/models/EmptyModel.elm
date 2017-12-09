module EmptyModel exposing (..)

import Material
import Html exposing (Html)

import XMapTypes exposing (..)
import ProjectModel exposing (..)
import Calculation exposing (..)
import InternalMessages exposing (..)

emptyTableConfiguration : TableConfiguration
emptyTableConfiguration = {
    columnsWidths = []
    }

emptyViewEditorModel : ViewEditorModel
emptyViewEditorModel = {
    viewName = Nothing
    , viewToEdit = Nothing
    , newViewName = ""
    , labelEditing = ""
    , rowToAddTo = 0
    }

emptyXMapEditorModel : XMapEditorModel
emptyXMapEditorModel = {
       xmapName = Nothing
       , xmapType = TypeDouble
       , xmapToEdit = Nothing
       , xmapEditing = Nothing
       , newXmapName = ""
       , tableConf = emptyTableConfiguration
    }

emptyCalculationEditorModel : CalculationEditorModel
emptyCalculationEditorModel = {
    calculationName = Nothing
    , resultMapName = Nothing
    , calculationFormulaText = Nothing
    , newCalculationName = ""
    , operationMode = Union
    }

emptyModel : Model
emptyModel = { openProjects = []
               , allProjects = []
               , currentProject = Nothing
               , currentView = Nothing
               , newProjectName = ""
               , messages = ["Client started"]
               , mdl =Material.model
               , xmapEditorModel = emptyXMapEditorModel
               , viewEditorModel = emptyViewEditorModel
               , calculationEditorModel = emptyCalculationEditorModel
               , currentProjectForm = ViewsForm
               , mapsInProject = []
               , functions = Nothing
               }

