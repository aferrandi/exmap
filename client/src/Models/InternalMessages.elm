module Models.InternalMessages exposing (..)

import Types.Calculation exposing (..)
import Types.Project exposing (..)
import Types.Views exposing (..)
import Types.XMapTypes exposing (..)


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
    | SwitchCategoryTo OperationCategory
    | TextToCalculationTextArea String
    | TextToResultNameText String
    | AddMapToCalculation XMapName
    | AddOperationToCalculation OperationId
    | ChangeOperationMode OperationMode
    | ChangeMapType XMapType
    | AddItemToView Int ViewItem
    | NewCalculationWithName CalculationName
    | NewViewWithName ViewName
    | NewMapWithName XMapName XMapType
    | NewProjectWithName ProjectName
    | ChangeViewEditSelectedRow Int
    | AddRowToView
    | ShowDialog String
    | CloseDialog
    | CloseDialogWithError String
