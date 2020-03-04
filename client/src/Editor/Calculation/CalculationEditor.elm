module Editor.Calculation.CalculationEditor exposing (viewCalculationsEditor)

import Editor.Calculation.FunctionChooser exposing (viewFunctions)
import Material.Select as Select
import Types.Calculation exposing (..)
import Html exposing (Html, div, text)
import Models.InternalMessages exposing (..)
import Transform.MapsExtraction exposing (xmapNameFromString, xmapNameToString)
import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Material.Options as Options
import Material.TextField as TextField
import Display.MdcIndexes exposing (..)
import Display.NameDialog exposing (nameDialog)
import Transform.NameParser exposing (..)
import Types.Project exposing (..)
import Models.ProjectModel exposing (..)
import Display.UIWrapper exposing (..)
import Models.WebMessages exposing (WebRequest(..))
import Transform.TypeConversion exposing (..)
import List.Extra as ListX
import Types.XMapTypes exposing (XMapName)

viewCalculationsEditor : Model -> ProjectModel -> Html Msg
viewCalculationsEditor model pm =
            LayoutGrid.view [ heightInView model.ui.heights.viewCalculationsEditor ]
                [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span2Desktop, LayoutGrid.span1Phone] [
                div []
                        [ calculationsInProjectList model pm
                        , newCalculationButton model
                        ]
                    ]
                , LayoutGrid.cell [LayoutGrid.span6Tablet, LayoutGrid.span10Desktop, LayoutGrid.span3Phone]
                    [ viewCalculationEditor model pm ]
                ]

title : Model -> String
title model =
            case model.calculationEditorModel.calculationName of
                Just calculationName -> "Editing: " ++ calculationName
                Nothing -> "Calculation Editor"

viewCalculationEditor : Model -> ProjectModel -> Html Msg
viewCalculationEditor model pm =
    case model.calculationEditorModel.calculationName of
        Just cn -> viewEditorForCalculation model pm cn
        Nothing -> div [] []

viewEditorForCalculation : Model -> ProjectModel -> CalculationName -> Html Msg
viewEditorForCalculation model pm cn =
    div []
        [ LayoutGrid.inner [  ]
            [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span3Desktop, LayoutGrid.span1Phone] [ resultMapNameText model ]
            , LayoutGrid.cell [LayoutGrid.span3Tablet, LayoutGrid.span5Desktop, LayoutGrid.span2Phone] [ operationNameChoice model ]
            , LayoutGrid.cell [LayoutGrid.span3Tablet, LayoutGrid.span4Desktop, LayoutGrid.span1Phone]
                [  ]
            ]
        , LayoutGrid.view [ heightInView model.ui.heights.viewEditorForCalculation ]
            [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span3Desktop, LayoutGrid.span1Phone] [ mapsInProjectListForCalculation model ]
            , LayoutGrid.cell [LayoutGrid.span3Tablet, LayoutGrid.span5Desktop, LayoutGrid.span2Phone] [ calculationTextArea model ]
            , LayoutGrid.cell [LayoutGrid.span3Tablet, LayoutGrid.span4Desktop, LayoutGrid.span1Phone] [ viewFunctions model ]
            ]
        , LayoutGrid.view [  ]
            [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span3Desktop, LayoutGrid.span1Phone] [ ]
            , LayoutGrid.cell [LayoutGrid.span3Tablet, LayoutGrid.span5Desktop, LayoutGrid.span2Phone] [  ]
            , LayoutGrid.cell [LayoutGrid.span3Tablet, LayoutGrid.span4Desktop, LayoutGrid.span1Phone]
                [ storeButton model pm ]
            ]
        ]

storeButton : Model -> ProjectModel -> Html Msg
storeButton model pm =
    storeCalculation pm model.calculationEditorModel |> buttonClick model (makeIndex calcEditorIdx "btnStr") "Store Calculation"


storeCalculation : ProjectModel -> CalculationEditorModel -> Msg
storeCalculation pm cm =
    case cm.calculationName of
        Just cn -> case cm.calculationFormulaText of
                Just ft -> case cm.resultMapName of
                        Just rns -> case xmapNameFromString rns of
                                Ok rn -> storeCalculationWithData pm.project.projectName cm cn rn ft
                                Err e -> Internal (ShowMessage e)
                        Nothing -> Internal (ShowMessage "Please enter a result map name")
                Nothing -> Internal (ShowMessage "Please enter a formula")
        Nothing -> Internal (ShowMessage "Please enter a calculation name")

storeCalculationWithData : ProjectName -> CalculationEditorModel -> CalculationName -> XMapName -> CalculationFormulaText -> Msg
storeCalculationWithData pn cm cn rn ft =
        let cs =  { calculationName = cn
                    , resultName = rn
                    , formulaText = ft
                    , operationMode = cm.operationMode
                    }
        in
            if cm.isNew then
                WRAddCalculation pn cs |> Send
            else
                WRUpdateCalculation pn cs |> Send

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
        div []
        [
            titleWithIcon (title model) "functions" "Green",
            Lists.ul Mdc (makeIndex calcEditorIdx "lstClcInPrj")
                    model.mdc
                     ([ Lists.onSelectListItem sendCalculation ] ++(scrollableListStyle model.ui.heights.calculationsInProjectList) )
                     (List.map listItem pm.project.calculations)
        ]



mapsInProjectListForCalculation : Model -> Html Msg
mapsInProjectListForCalculation model =
    let
        selectItem index = Internal (SelectMapIndexForCalculation index)
        buildMsg index = sendListMsg (\mn -> (Internal (AddMapToCalculation mn))) model.mapsInProject index
        sendAddItem = Maybe.withDefault None (Maybe.map buildMsg model.calculationEditorModel.selectedMapIdx)
        listItem mn =
            Lists.li []
                [
                    Lists.graphicIcon  [] "list",
                    text (xmapNameToString mn)
                ]
    in
        div []
            [
                Lists.ul Mdc
                        (makeIndex calcEditorIdx "lstMapInPrj")
                        model.mdc
                        ((Lists.onSelectListItem selectItem) :: (scrollableListStyle model.ui.heights.mapsInProjectListForCalculation))
                        (List.map listItem model.mapsInProject)
                , buttonClick model (makeIndex viewEditorIdx "btnAddMap") "Add map"  sendAddItem
            ]

calculationTextArea : Model -> Html Msg
calculationTextArea model =
    TextField.view Mdc
        (makeIndex calcEditorIdx "txaClc")
        model.mdc
        [ TextField.label "Enter the formula"
        , TextField.textarea
        , heightInView model.ui.heights.calculationTextArea
        , TextField.rows 25
        , TextField.value (Maybe.withDefault "" model.calculationEditorModel.calculationFormulaText)
        , Options.onInput (\s -> Internal (TextToCalculationTextArea s))
        , useWholeWidth
        ]
        []

resultMapNameText : Model -> Html Msg
resultMapNameText model =
    TextField.view Mdc
        (makeIndex calcEditorIdx "txtResMapNme")
        model.mdc
        [ TextField.label "Enter the result map name"
        , TextField.value (Maybe.withDefault "" model.calculationEditorModel.resultMapName)
        , Options.onInput (\s -> Internal (TextToResultNameText s))
        ]
        []

operationNameChoice : Model -> Html Msg
operationNameChoice model =
    let
        hasMode m = model.calculationEditorModel.operationMode == m
        modes = [Union, Intersection]
        texts = ["Union", "Intersection"]
        changeMapTypeFromText txt = Internal (ChangeOperationMode (textToEnum modes texts txt |> Maybe.withDefault Union))
        selectOptions m = Select.value (enumToText modes texts m |> Maybe.withDefault "") :: (if hasMode m then [Select.selected] else [])
    in
        Select.view Mdc
            (makeIndex mapEditorIdx "selOperationName")
            model.mdc
            [ Select.label "Mode"
            , Select.selectedText (enumToText modes texts model.calculationEditorModel.operationMode |> Maybe.withDefault "")
            , Select.onSelect changeMapTypeFromText
             ]
             (ListX.zip modes texts |> List.map (\(m, txt) -> Select.option (selectOptions m) [ text txt]))

newCalculationButton : Model -> Html Msg
newCalculationButton model =
    let
        calculationEditorModel = model.calculationEditorModel
        newCalculationMessage =
            case nameFromString calculationEditorModel.newCalculationName of
                Ok newCalculationName -> Internal (NewCalculationWithName newCalculationName)
                Err err -> Internal (CloseDialogWithError err)
        idxDialog = makeIndex calcEditorIdx "dlgNewClc"
    in
        div []
            [
              nameDialog idxDialog model "New Calculation" (\s -> Internal (UpdateCalculationName s)) newCalculationMessage
            , buttonClick model (makeIndex calcEditorIdx "btnNewClc") "New Calculation"  (Internal (ShowDialog idxDialog))
            ]
