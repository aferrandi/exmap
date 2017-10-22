module InternalMessages exposing (..)

import XMapTypes exposing (..)
import Project exposing (..)
import Views exposing (..)
import Calculation exposing (..)

type ProjectViewType =
    ViewsView
    | MapEditorView
    | ViewEditorView
    | CalculationEditorView

type InternalMsg =
  SelectProjectTab Int
  | SelectViewTab Int
  | MapToTextArea
  | MapToTable
  | TextToMapTextArea String
  | UpdateMapName String
  | UpdateCalculationName String
  | UpdateViewName String
  | UpdateProjectName String
  | ShowMessage String
  | SwitchProjectViewTo ProjectViewType
  | TextToCalculationTextArea String
  | TextToResultNameText String
  | AddMapToCalculation XMapName
  | AddApplicationToCalculation ApplicationName
  | AddOperationToCalculation OperationName
  | ChangeOperationMode OperationMode
  | AddItemToView Int ViewItem
  | NewCalculationWithName CalculationName