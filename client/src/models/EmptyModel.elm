module EmptyModel exposing (..)

import Material
import Html exposing (Html)

import XMapTypes exposing (..)
import ProjectModel exposing (..)
import Calculation exposing (..)
import InternalMessages exposing (..)


emptyViewEditorModel : ViewEditorModel
emptyViewEditorModel = {
    viewName = Nothing
    , viewToEdit = Nothing
    , newViewName = ""
    }

emptyXMapEditorModel : XMapEditorModel
emptyXMapEditorModel = {
       xmapName = Nothing
       , xmapType = TypeDouble
       , xmapToEdit = Nothing
       , xmapEditing = Nothing
       , newXmapName = ""
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
               , newProjectName = ""
               , messages = ["Client started"]
               , mdl =Material.model
               , projectTab = 0
               , viewTab = 0
               , xmapEditorModel = emptyXMapEditorModel
               , viewEditorModel = emptyViewEditorModel
               , calculationEditorModel = emptyCalculationEditorModel
               , currentProjectView = ViewsView
               , mapsInProject = []
               , functions = Nothing
               }

