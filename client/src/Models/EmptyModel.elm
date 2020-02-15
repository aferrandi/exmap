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
    , selectedMapIdx = Nothing
    , labelEditing = ""
    , rowToAddTo = 0
    , lastViewEditItemId = 0
    , checkedViewEditItems = Dict.empty
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

emptyHeights: Heights
emptyHeights =
    { viewProjects = 90
    , viewAllProjectsList = 75
    , viewMessages = 10
    , viewViews = 70
    , viewAllViewsList = 70
    , viewCalculationsEditor = 80
    , viewEditorForCalculation = 65
    , calculationTextArea = 60
    , calculationsInProjectList = 60
    , mapsInProjectList = 60
    , functionsNamesList = 55
    , mapEditorView = 85
    , mapEditorViewForMap = 65
    , mapEditorTextArea = 65
    , mapEditorTableFull = 65
    , mapEditorMapList = 60
    , viewViewsEditor = 80
    , viewEditorForView = 65
    , viewEditorViewsList = 60
    , viewEditorMapList = 40
    , viewView = 75
    }

emptyUI: UI
emptyUI = {
    heights = emptyHeights
    }

emptyModel : Model
emptyModel =
    {
    ui = emptyUI
    , openProjects = []
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