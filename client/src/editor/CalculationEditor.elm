module CalculationEditor exposing (viewCalculationsEditor)

import Calculation exposing (..)
import EmptyModel exposing (emptyFunctionModel)
import Html exposing (Html, div, text)
import InternalMessages exposing (..)
import MapsExtraction exposing (xmapNameFromString, xmapNameToString)
import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Material.Options as Options
import Material.TextField as TextField
import MdlIndexes exposing (..)
import NameDialog exposing (nameDialog)
import NameParser exposing (..)
import Project exposing (..)
import ProjectModel exposing (..)
import UIWrapper exposing (..)
import WebMessages exposing (WebRequest(..))


viewCalculationsEditor : Model -> ProjectModel -> Html Msg
viewCalculationsEditor model pm =
    let
        title =
            case model.calculationEditorModel.calculationName of
                Just calculationName -> "Editing calculation: " ++ calculationName
                Nothing -> "Calculation Editor"
    in
        div []
            [ titleWithIcon title "functions" "Green"
            , LayoutGrid.view [ heightInView 70 ]
                [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span2Desktop, LayoutGrid.span1Phone] [ div [] [ calculationsInProjectList model pm, newCalculationButton model ] ]
                , LayoutGrid.cell [LayoutGrid.span6Tablet, LayoutGrid.span10Desktop, LayoutGrid.span3Phone] [ viewCalculationEditor model pm ]
                ]
            ]


viewCalculationEditor : Model -> ProjectModel -> Html Msg
viewCalculationEditor model pm =
    case model.calculationEditorModel.calculationName of
        Just cn -> viewEditorForCalculation model pm cn
        Nothing -> div [] []

viewEditorForCalculation : Model -> ProjectModel -> CalculationName -> Html Msg
viewEditorForCalculation model pm cn =
    div []
        [ LayoutGrid.view []
            [ LayoutGrid.cell [LayoutGrid.span4Tablet, LayoutGrid.span4Desktop, LayoutGrid.span2Phone] [ resultMapNameText model ]
            , LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span3Desktop, LayoutGrid.span1Phone] [ operationNameChoice model ]
            ]
        , LayoutGrid.view [ heightInView 50 ]
            [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span3Desktop, LayoutGrid.span1Phone] [ mapsInProjectList model ]
            , LayoutGrid.cell [LayoutGrid.span4Tablet, LayoutGrid.span4Desktop, LayoutGrid.span2Phone] [ calculationTextArea model ]
            , LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span3Desktop, LayoutGrid.span1Phone] [ functionsList model ]
            ]
            -- LayoutGrid.noSpacing
        , LayoutGrid.view [  ]
            [ LayoutGrid.cell [LayoutGrid.span3Tablet, LayoutGrid.span8Desktop, LayoutGrid.span4Phone]  []
            , LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span4Desktop, LayoutGrid.span2Phone]  [ storeButton model pm ]
            ]
        ]


storeButton : Model -> ProjectModel -> Html Msg
storeButton model pm =
    storeCalculation pm model.calculationEditorModel |> buttonClick model (makeIndex calcEditorIdx 1) "Store"


storeCalculation : ProjectModel -> CalculationEditorModel -> Msg
storeCalculation pm cm =
    case cm.calculationName of
        Just cn ->
            case cm.calculationFormulaText of
                Just ft ->
                    case cm.resultMapName of
                        Just rns ->
                            case xmapNameFromString rns of
                                Ok rn ->
                                    Send
                                        (WRStoreCalculation pm.project.projectName
                                            { calculationName = cn
                                            , resultName = rn
                                            , formulaText = ft
                                            , operationMode = cm.operationMode
                                            }
                                        )

                                Err e ->
                                    Internal (ShowMessage e)

                        Nothing ->
                            Internal (ShowMessage "Please enter a result map name")

                Nothing ->
                    Internal (ShowMessage "Please enter a formula")

        Nothing ->
            Internal (ShowMessage "Please enter a calculation name")


calculationsInProjectList : Model -> ProjectModel -> Html Msg
calculationsInProjectList model pm =
    let
        projectName = pm.project.projectName
        sendCalculation index = sendListMsg (\cn -> Send (WRLoadCalculation projectName cn)) pm.project.calculations index
        listItem cn =
            Lists.li []
                [
                    Lists.graphicIcon [] "list",
                    text cn
                ]
    in
        Lists.ul Mdc (makeIndex calcEditorIdx 10)
                model.mdc
                 ([ Lists.onSelectListItem sendCalculation ] ++(scrollableListStyle 55) )
                 (List.map listItem pm.project.calculations)


mapsInProjectList : Model -> Html Msg
mapsInProjectList model =
    let
        sendAddMap index = sendListMsg (\mn -> (Internal (AddMapToCalculation mn))) model.mapsInProject index
        listItem mn =
            Lists.li []
                [
                    Lists.graphicIcon  [] "list",
                    text (xmapNameToString mn)
                ]
    in
        Lists.ul Mdc
                (makeIndex calcEditorIdx 9)
                model.mdc
                ((Lists.onSelectListItem sendAddMap) :: (scrollableListStyle 40))
                (List.map listItem model.mapsInProject)


functionsList : Model -> Html Msg
functionsList model =
    let
        sendAddOperation index = sendListMsg (\on -> (Internal (AddOperationToCalculation on))) functions.operationNames index
        operationListItem on =
            Lists.li []
                [
                  Lists.graphicIcon  [] "play_arrow",
                  text on
                ]
        functions =
            Maybe.withDefault emptyFunctionModel model.functions
        operationList =
            List.map operationListItem functions.operationNames
    in
    Lists.ul Mdc
        (makeIndex calcEditorIdx 8)
        model.mdc
        (( Lists.onSelectListItem sendAddOperation) :: (scrollableListStyle 40))
        operationList


calculationTextArea : Model -> Html Msg
calculationTextArea model =
    TextField.view Mdc
        (makeIndex calcEditorIdx 2)
        model.mdc
        [ TextField.label "Enter the formula"
        , TextField.textarea
        , heightInView 40
        , TextField.rows 20
        , TextField.value (Maybe.withDefault "" model.calculationEditorModel.calculationFormulaText)
        , Options.onInput (\s -> Internal (TextToCalculationTextArea s))
        ]
        []


resultMapNameText : Model -> Html Msg
resultMapNameText model =
    TextField.view Mdc
        (makeIndex calcEditorIdx 3)
        model.mdc
        [ TextField.label "Enter the result map name"
        -- , TextField.floatingLabel
        , TextField.value (Maybe.withDefault "" model.calculationEditorModel.resultMapName)
        , Options.onInput (\s -> Internal (TextToResultNameText s))
        ]
        []


operationNameChoice : Model -> Html Msg
operationNameChoice model =
    let
        hasMode m =
            model.calculationEditorModel.operationMode == m
    in
    div []
        [ radio model (makeIndex calcEditorIdx 4) "Union" "operationName" (hasMode Union) (Internal (ChangeOperationMode Union))
        , radio model (makeIndex calcEditorIdx 5) "Intersection" "operationName" (hasMode Intersection) (Internal (ChangeOperationMode Intersection))
        ]


newCalculationButton : Model -> Html Msg
newCalculationButton model =
    let
        calculationEditorModel =
            model.calculationEditorModel

        newCalculationMessage =
            case nameFromString calculationEditorModel.newCalculationName of
                Ok newCalculationName -> Internal (NewCalculationWithName newCalculationName)
                Err err -> Internal (CloseDialogWithError err)
    in
    div []
        [
          nameDialog (makeIndex calcEditorIdx 8) model "New Calculation" (\s -> Internal (UpdateCalculationName s)) newCalculationMessage
        , buttonClick model (makeIndex calcEditorIdx 7) "New Calculation"  (Internal (ShowDialog (makeIndex calcEditorIdx 8)))
        ]
