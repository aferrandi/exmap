module Handler.InternalCalculationMessageUpdate exposing (..)

import Dict
import Handler.ModelUpdate exposing (updateCalculationEditorModel)
import Models.EmptyModel exposing (emptyCalculationEditorModel)
import Models.ProjectModel exposing (Model, closeDialog)
import Types.Calculation exposing (CalculationName, OperationCategory, OperationId, OperationMode, OperationType, ParameterType(..), operationIdToTuple)

handleUpdateCalculationName : Model -> String -> Model
handleUpdateCalculationName model s =
    updateCalculationEditorModel model (\cm -> { cm | newCalculationName = s })

handleNewCalculationWithName : Model -> CalculationName -> Model
handleNewCalculationWithName model cn =
    closeDialog { model | calculationEditorModel = { emptyCalculationEditorModel | calculationName = Just cn, resultMapName = Just cn, isNew = True } }

handleChangeOperationMode : Model -> OperationMode -> Model
handleChangeOperationMode model om =
    updateCalculationEditorModel model (\cm -> { cm | operationMode = om })

handleChangeOperationsMatch: Model -> String -> Model
handleChangeOperationsMatch model om =
    updateCalculationEditorModel model (\cm -> { cm | operationsMatch = om })

handleSwitchCategoryTo : Model -> OperationCategory -> Model
handleSwitchCategoryTo model ct = { model | currentCategory = Just ct }


handleTextToResultNameText : Model -> String -> Model
handleTextToResultNameText model mn =
    updateCalculationEditorModel model (\cm -> { cm | resultMapName = Just mn })

handleAddOperationToCalculation : Model -> OperationId -> Model
handleAddOperationToCalculation model id =
    let
        text = model.functions
                |> Maybe.andThen (\fm -> Dict.get (operationIdToTuple id) fm.typesById)
                |> Maybe.map operationTypeToText
    in
        appendToFormulaText model (Maybe.withDefault " " text)


handleSelectMapIndexForCalculationEdit : Model -> Int  -> Model
handleSelectMapIndexForCalculationEdit model idx =
    updateCalculationEditorModel model (\cm -> { cm | selectedMapIdx = Just idx})

handleSelectFunctionIndexForCalculationEdit : Model -> Int  -> Model
handleSelectFunctionIndexForCalculationEdit model idx =
   updateCalculationEditorModel model (\cm -> { cm | selectedFunctionIdx = Just idx})

appendToFormulaText : Model -> String -> Model
appendToFormulaText model s =
    let
        updateFormulaText cm = Maybe.withDefault "" cm.calculationFormulaText ++ " " ++ s
    in
        updateCalculationEditorModel model (\cm -> { cm | calculationFormulaText = Just (updateFormulaText cm) })


operationTypeToText : OperationType -> String
operationTypeToText ot =
    let
        insidePars s = "[" ++ s ++ "]"
    in
        ot.operationId.name ++ " " ++ String.join " " (List.map (parameterTypeToText >> insidePars) ot.parametersTypes)


parameterTypeToText t =
    case t of
        ParameterDouble -> "double"
        ParameterInt -> "int"
        ParameterString -> "string"
        ParameterBool -> "bool"
        ParameterAny -> "any"