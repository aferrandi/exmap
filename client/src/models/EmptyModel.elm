module EmptyModel exposing (..)

import Calculation exposing (..)
import Dict as Dict
import Html exposing (Html)
import InternalMessages exposing (..)
import Material
import ProjectModel exposing (..)
import XMapTypes exposing (..)


emptyTableConfiguration : TableConfiguration
emptyTableConfiguration =
    { columnsWidths = []
    }


emptyFunctionModel : FunctionsModel
emptyFunctionModel =
    { operationNames = []
    , typesByName = Dict.empty
    }


emptyViewEditorModel : ViewEditorModel
emptyViewEditorModel =
    { viewName = Nothing
    , viewToEdit = Nothing
    , newViewName = ""
    , labelEditing = ""
    , rowToAddTo = 0
    }


emptyXMapEditorModel : XMapEditorModel
emptyXMapEditorModel =
    { xmapName = Nothing
    , xmapType = TypeDouble
    , xmapToEdit = Nothing
    , xmapEditing = Nothing
    , newXmapName = ""
    , tableConf = emptyTableConfiguration
    }


emptyCalculationEditorModel : CalculationEditorModel
emptyCalculationEditorModel =
    { calculationName = Nothing
    , resultMapName = Nothing
    , calculationFormulaText = Nothing
    , newCalculationName = ""
    , operationMode = Union
    }


emptyModel : Model
emptyModel =
    { openProjects = []
    , allProjects = []
    , currentProject = Nothing
    , currentView = Nothing
    , newProjectName = ""
    , messages = [ "Client started" ]
    , mdc = Material.defaultModel
    , xmapEditorModel = emptyXMapEditorModel
    , viewEditorModel = emptyViewEditorModel
    , calculationEditorModel = emptyCalculationEditorModel
    , currentProjectForm = ViewsForm
    , mapsInProject = []
    , openDialog = Nothing
    , functions = Nothing
    }
