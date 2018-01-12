module CalculationEditor exposing (viewCalculationsEditor)

import Html exposing (Html,text, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class, style)
import Material.Tabs as Tabs
import Material.Icon as Icon
import Material.Dialog as Dialog
import Material.List as Lists
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Menu as Menu exposing (Item)
import Material.Grid as Grid exposing (Device(..))
import Material.Options as Options exposing (css)
import Material.Toggles as Toggles
import List.Extra as ListX exposing (transpose)

import Calculation exposing (..)
import Project exposing (..)
import XMapTypes exposing(..)
import Views exposing (..)
import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import NameParser exposing (..)
import ViewUI exposing (..)
import UIWrapper exposing (..)
import MapsExtraction exposing (xmapNameToString, xmapNameFromString)
import InternalMessages exposing (..)
import MdlIndexes exposing (..)


viewCalculationsEditor : Model -> ProjectModel -> Html Msg
viewCalculationsEditor model pm =
        let title = case model.calculationEditorModel.calculationName of
                                    Just calculationName -> "Editing calculation: " ++ calculationName
                                    Nothing -> "Calculation Editor"
        in div []
                [
                    titleWithIcon title "functions" Color.Green,
                    Grid.grid [heightInView 70]
                    [ cell 2 2 1 [ ]  [div[] [calculationsInProjectList model pm, newCalculationButton model]]
                    , cell 6 10 3 []  [viewCalculationEditor model pm]
                     ]
                 ]

viewCalculationEditor : Model -> ProjectModel -> Html Msg
viewCalculationEditor model pm = case model.calculationEditorModel.calculationName of
                                    Just cn -> viewEditorForCalculation model pm cn
                                    Nothing -> div [][]

viewEditorForCalculation : Model -> ProjectModel -> CalculationName -> Html Msg
viewEditorForCalculation model pm cn =
    div []
         [
             Grid.grid []
             [ cell 4 4 2 [] [resultMapNameText model]
             , cell 2 3 1 [ ]  [operationNameChoice model]
             ],
             Grid.grid [heightInView 50]
             [ cell 2 3 1 []  [mapsInProjectList model]
             , cell 4 4 2 [] [calculationTextArea model]
             , cell 2 3 1 []  [functionsList model]
             ],
             Grid.grid [ Grid.noSpacing]
                  [ cell 3 8 6 [] [ ]
                  , cell 1 4 2 [] [ storeButton model pm  ]
             ]
          ]


storeButton : Model -> ProjectModel -> Html Msg
storeButton model pm = storeCalculation pm model.calculationEditorModel |> buttonClick model [calcEditorIdx, 1] "Store"

storeCalculation : ProjectModel -> CalculationEditorModel -> Msg
storeCalculation pm cm = case cm.calculationName of
    Just cn -> case cm.calculationFormulaText of
        Just ft -> case cm.resultMapName of
            Just rns -> case xmapNameFromString rns of
                Ok rn -> Send (WRStoreCalculation pm.project.projectName {
                                calculationName = cn,
                                resultName = rn,
                                formulaText = ft,
                                operationMode = cm.operationMode})
                Err e -> Internal (ShowMessage e)
            Nothing -> Internal (ShowMessage "Please enter a result map name")
        Nothing -> Internal (ShowMessage "Please enter a formula")
    Nothing -> Internal (ShowMessage "Please enter a calculation name")

calculationsInProjectList : Model -> ProjectModel -> Html Msg
calculationsInProjectList model pm =
    let projectName = pm.project.projectName
        listItem cn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Send (WRLoadCalculation projectName cn)) ]
                               [ Lists.avatarIcon "list" [], text cn ]
                           ]
    in Lists.ul (scrollableListStyle 55) (List.map listItem (pm.project.calculations))


mapsInProjectList : Model -> Html Msg
mapsInProjectList model  =
    let listItem mn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Internal (AddMapToCalculation mn)) ]
                               [ Lists.avatarIcon "list" [], text (xmapNameToString mn) ]
                           ]
    in Lists.ul (scrollableListStyle 40) (List.map listItem (model.mapsInProject))

functionsList : Model -> Html Msg
functionsList model  =
    let applicationListItem an = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Internal (AddApplicationToCalculation an)) ]
                               [ Lists.avatarIcon "fast_forward" [], text an ]
                           ]
        operationListItem on = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Internal (AddOperationToCalculation on)) ]
                               [ Lists.avatarIcon "play_arrow" [], text on ]
                           ]
        functions = Maybe.withDefault { applications = [], operations= [] }  model.functions
        applicationList = List.map applicationListItem functions.applications
        operationList = List.map operationListItem functions.operations
    in Lists.ul (scrollableListStyle 40) ( List.append applicationList operationList)

calculationTextArea : Model  -> Html Msg
calculationTextArea model = Textfield.render Mdl [calcEditorIdx, 2] model.mdl
                              [ Textfield.label "Enter the formula"
                              , Textfield.textarea
                              , Textfield.rows 20
                              , Textfield.value (Maybe.withDefault "" model.calculationEditorModel.calculationFormulaText)
                              , Options.onInput (\s -> Internal (TextToCalculationTextArea s))
                              ]
                              []

resultMapNameText : Model  -> Html Msg
resultMapNameText model = Textfield.render Mdl [calcEditorIdx, 3] model.mdl
                              [ Textfield.label "Enter the result map name"
                              , Textfield.floatingLabel
                              , Textfield.value (Maybe.withDefault "" model.calculationEditorModel.resultMapName )
                              , Options.onInput (\s -> Internal (TextToResultNameText s))
                              ]
                              []

operationNameChoice : Model  -> Html Msg
operationNameChoice model =
  let hasMode m = model.calculationEditorModel.operationMode == m
  in div []
  [
    toggle model [calcEditorIdx, 4] "Union" "operationName" (hasMode Union) (Internal (ChangeOperationMode Union)),
    toggle model [calcEditorIdx, 5] "Intersection" "operationName" (hasMode Intersection)  (Internal (ChangeOperationMode Intersection))
    ]

newCalculationButton : Model -> Html Msg
newCalculationButton model =
    let calculationEditorModel = model.calculationEditorModel
        newCalculationMessage = case nameFromString calculationEditorModel.newCalculationName of
                                    Ok newCalculationName -> Internal (NewCalculationWithName calculationEditorModel.newCalculationName)
                                    Err err -> Internal (ShowMessage err)
    in div []
        [ Textfield.render Mdl [calcEditorIdx, 6] model.mdl
                                             [ Textfield.label "New calculation name"
                                             , Textfield.floatingLabel
                                             , Textfield.text_
                                             , Textfield.value calculationEditorModel.newCalculationName
                                             , Options.onInput (\s -> Internal (UpdateCalculationName s))
                                             ] []
        , buttonClick model [calcEditorIdx, 7] "New calculation" newCalculationMessage
        ]
