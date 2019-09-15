module InternalMessages exposing (..)

import Calculation exposing (..)
import Project exposing (..)
import Views exposing (..)
import XMapTypes exposing (..)


type ProjectFormType
    = ViewsForm
    | MapEditorForm
    | ViewEditorForm
    | CalculationEditorForm


type InternalMsg
    = MapToTextArea
    | MapToTable
    | OpenProject ProjectName
    | OpenView ViewName
    | TextToMapTextArea String
    | UpdateMapName String
    | UpdateCalculationName String
    | UpdateViewName String
    | UpdateProjectName String
    | UpdateViewLabel String
    | ShowMessage String
    | ShowMapInEditor XMapName
    | SwitchProjectViewTo ProjectFormType
    | TextToCalculationTextArea String
    | TextToResultNameText String
    | AddMapToCalculation XMapName
    | AddOperationToCalculation OperationName
    | ChangeOperationMode OperationMode
    | ChangeMapType XMapType
    | AddItemToView Int ViewItem
    | NewCalculationWithName CalculationName
    | NewViewWithName ViewName
    | NewMapWithName XMapName XMapType
    | ChangeViewEditSelectedRow Int
    | AddRowToView
    | ShowDialog String
    | CloseDialog
