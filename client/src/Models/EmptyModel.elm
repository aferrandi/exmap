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
    , isNew = False
    }

emptyXMapEditorModel : XMapEditorModel
emptyXMapEditorModel =
    { xmapName = Nothing
    , xmapType = TypeDouble
    , xmapToEdit = Nothing
    , xmapEditing = Nothing
    , newXmapName = ""
    , tableConf = emptyTableConfiguration
    , isNew = False
    }

emptyCalculationEditorModel : CalculationEditorModel
emptyCalculationEditorModel =
    { calculationName = Nothing
    , resultMapName = Nothing
    , calculationFormulaText = Nothing
    , newCalculationName = ""
    , operationMode = Union
    , operationsMatch = ""
    , isNew = False
    }

emptyHeights: Heights
emptyHeights =
    { viewProjects = 90
    , viewAllProjectsList = 80
    , viewMessages = 50
    , viewViews = 80
    , viewAllViewsList = 80
    , viewCalculationsEditor = 90
    , viewEditorForCalculation = 65
    , calculationTextArea = 65
    , calculationsInProjectList = 70
    , mapsInProjectListForCalculation = 63
    , functionsNamesList = 55
    , mapEditorView = 90
    , mapEditorViewForMap = 75
    , mapEditorTextArea = 75
    , mapEditorTableFull = 75
    , mapEditorMapList = 70
    , viewViewsEditor = 90
    , viewEditorForView = 75
    , viewEditorViewsList = 70
    , viewEditorMapList = 55
    , viewEditRows = 70
    , viewView = 85
    , newMapDialog = 50
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