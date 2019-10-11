module Editor.CalculationEditor exposing (viewCalculationsEditor)

import Editor.FunctionChooser exposing (viewFunctions)
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
        , LayoutGrid.view [ heightInView 45 ]
            [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span3Desktop, LayoutGrid.span1Phone] [ mapsInProjectList model ]
            , LayoutGrid.cell [LayoutGrid.span4Tablet, LayoutGrid.span4Desktop, LayoutGrid.span2Phone] [ calculationTextArea model ]
            , LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span3Desktop, LayoutGrid.span1Phone] [ viewFunctions model ]
            ]
        , LayoutGrid.view [  ]
            [ LayoutGrid.cell [LayoutGrid.span3Tablet, LayoutGrid.span8Desktop, LayoutGrid.span4Phone]  []
            , LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span4Desktop, LayoutGrid.span2Phone]  [ storeButton model pm ]
            ]
        ]

storeButton : Model -> ProjectModel -> Html Msg
storeButton model pm =
    storeCalculation pm model.calculationEditorModel |> buttonClick model (makeIndex calcEditorIdx "btnStr") "Store"


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
        Lists.ul Mdc (makeIndex calcEditorIdx "lstClcInPrj")
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
                (makeIndex calcEditorIdx "lstMapInPrj")
                model.mdc
                ((Lists.onSelectListItem sendAddMap) :: (scrollableListStyle 40))
                (List.map listItem model.mapsInProject)




calculationTextArea : Model -> Html Msg
calculationTextArea model =
    TextField.view Mdc
        (makeIndex calcEditorIdx "txaClc")
        model.mdc
        [ TextField.label "Enter the formula"
        , TextField.textarea
        , heightInView 40
        , TextField.rows 20
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
        hasMode m =
            model.calculationEditorModel.operationMode == m
    in
        div []
            [ radio model (makeIndex calcEditorIdx "opeNamUni") "Union" "operationName" (hasMode Union) (Internal (ChangeOperationMode Union))
            , radio model (makeIndex calcEditorIdx "opeNamInt") "Intersection" "operationName" (hasMode Intersection) (Internal (ChangeOperationMode Intersection))
            ]


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
