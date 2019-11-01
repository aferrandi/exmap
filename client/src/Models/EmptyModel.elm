module Models.EmptyModel exposing (..)

import Types.Calculation exposing (..)
import Dict as Dict
import Models.InternalMessages exposing (..)
import Material
import Models.ProjectModel exposing (..)
import Types.XMapTypes exposing (..)

emptyTableConfiguration : TableConfiguration
emptyTableConfiguration =
    { columnsWidths = []
    }

emptyFunctionModel : FunctionsModel
emptyFunctionModel =
    { idsByCategory = Dict.empty
    , typesById = Dict.empty
    }

emptyViewEditorModel : ViewEditorModel
emptyViewEditorModel =
    { viewName = Nothing
    , viewToEdit = Nothing
    , newViewName = ""
    , labelEditing = ""
    , rowToAddTo = 0
    , selectedViewCells = Dict.empty
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
    , currentCategory = Nothing
    , mapsInProject = []
    , openDialog = Nothing
    , functions = Nothing
    }