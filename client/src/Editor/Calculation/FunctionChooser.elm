module Editor.Calculation.FunctionChooser exposing (viewFunctions)

import Material.LayoutGrid as LayoutGrid
import Material.Select as Select
import Material.TextField as TextField
import Types.Calculation exposing (..)
import Models.EmptyModel exposing (emptyFunctionModel)
import Html exposing (Html, div, text)
import Models.InternalMessages exposing (..)
import Material.List as Lists
import Material.Options as Options
import Display.MdcIndexes exposing (..)
import Models.ProjectModel exposing (..)
import Display.UIWrapper exposing (..)
import Dict as Dict

viewFunctions : Model -> Html Msg
viewFunctions model  =
    div []
        [
            LayoutGrid.inner [ ]
                [ LayoutGrid.cell [LayoutGrid.span4Tablet, LayoutGrid.span6Desktop, LayoutGrid.span2Phone] [ categoryChoice model ]
                , LayoutGrid.cell [LayoutGrid.span4Tablet, LayoutGrid.span6Desktop, LayoutGrid.span2Phone] [ matchText model ]
                ]
        , functionsNamesList model
        ]

functions : Model -> FunctionsModel
functions model = Maybe.withDefault emptyFunctionModel model.functions

categories : Model -> List OperationCategory
categories model = Dict.keys (functions model).idsByCategory

categoryChoice : Model -> Html Msg
categoryChoice model =
    let
        cats = categories model
        selectOptions cat = Select.value cat :: (Maybe.andThen(\c -> if c == cat then Just [Select.selected] else Nothing) model.currentCategory |> Maybe.withDefault [])
    in
        Select.view Mdc
            (makeIndex calcEditorIdx "selCategory")
            model.mdc
            [ Select.label "Category"
            , Select.selectedText (Maybe.withDefault "" model.currentCategory)
            , Select.onSelect (\cat -> (Internal (SwitchCategoryTo cat)))
             ]
            (List.map (\cat -> Select.option  (selectOptions cat) [text cat]) cats)

matchText : Model -> Html Msg
matchText model =
    TextField.view Mdc
        (makeIndex calcEditorIdx "txtMatch")
        model.mdc
        [ TextField.label "Search"
        , TextField.value model.calculationEditorModel.operationsMatch
        , Options.onInput (\s -> Internal (ChangeOperationsMatch s))
        ]
        []

functionsNamesList : Model -> Html Msg
functionsNamesList model  =
    let
        allOperations = Dict.values (functions model).idsByCategory |> List.concat |> List.sortBy (\o -> o.name)
        operationsSelected =
            case model.currentCategory of
                Nothing -> allOperations
                Just currentCategory -> Dict.get currentCategory (functions model).idsByCategory |> Maybe.withDefault allOperations
        toMatch = String.trim model.calculationEditorModel.operationsMatch |> String.toLower
        operationToShow = case toMatch of
            "" -> operationsSelected
            _ -> List.filter (\o -> String.toLower o.name |> String.contains toMatch) operationsSelected
        selectItem index = Internal (SelectFunctionIndexForCalculation index)
        buildMsg index = sendListMsg (\on -> (Internal (AddOperationToCalculation on))) operationToShow index
        sendAddItem = Maybe.withDefault None (Maybe.map buildMsg model.calculationEditorModel.selectedFunctionIdx)
        operationListItem on =
            Lists.li []
                [
                  Lists.graphicIcon  [] "play_arrow",
                  text on.name
                ]
        operationList = List.map operationListItem operationToShow
    in
        div []
        [
            Lists.ul Mdc
                (makeIndex calcEditorIdx "lstFnc")
                model.mdc
                ([ Lists.onSelectListItem selectItem, Options.onDoubleClick sendAddItem] ++ (scrollableListStyle model.ui.heights.functionsNamesList))
                operationList
            , buttonClick model (makeIndex viewEditorIdx "btnAddFunction") "Add function"  sendAddItem
        ]
