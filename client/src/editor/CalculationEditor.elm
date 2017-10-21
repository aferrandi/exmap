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
import ViewUI exposing (..)
import UIWrapper exposing (..)
import Stretch exposing (..)
import MapsExtraction exposing (xmapNameToString)
import InternalMessages exposing (..)


viewCalculationsEditor : Model -> ProjectModel -> Html Msg
viewCalculationsEditor model pm = div []
                                [
                                    titleWithIcon "Calculation Editor" "functions" Color.Green,
                                    Grid.grid [heightInView 70]
                                    [ cell 2 2 1 [ Color.background lighterGrey]  [calculationsInProjectList model pm]
                                    , cell 6 10 3 []  [viewCalculationEditor model pm]
                                     ]
                                 ]

viewCalculationEditor : Model -> ProjectModel -> Html Msg
viewCalculationEditor model pm = case model.calculationEditorModel.calculationName of
                                    Just cn -> viewEditorForCalculation model pm
                                    Nothing -> div [][]

viewEditorForCalculation : Model -> ProjectModel -> Html Msg
viewEditorForCalculation model pm = div []
                                     [
                                         titleWithIcon "Calculation " "functions" Color.Green,
                                         Grid.grid [heightInView 60]
                                         [ cell 2 3 1 [ Color.background lighterGrey]  [mapsInProjectList model]
                                         , cell 4 4 2 [] [
                                                     div [] [resultMapNameText model],
                                                     div [] [operationNameChoice model],
                                                     div [] [calculationTextArea model]
                                                     ]

                                         , cell 2 3 1 [ Color.background lighterGrey]  [functionsList model]
                                      ]
                                      ]



calculationsInProjectList : Model -> ProjectModel -> Html Msg
calculationsInProjectList model pm =
    let projectName = pm.project.projectName
        listItem cn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Send (WRLoadCalculation projectName cn)) ]
                               [ Lists.avatarIcon "list" [], text cn ]
                           ]
    in Lists.ul [] (List.map listItem (pm.project.calculations))


mapsInProjectList : Model -> Html Msg
mapsInProjectList model  =
    let listItem mn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Internal (AddMapToCalculation mn)) ]
                               [ Lists.avatarIcon "list" [], text (xmapNameToString mn) ]
                           ]
    in Lists.ul [] (List.map listItem (model.mapsInProject))

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
    in Lists.ul [] ( List.append applicationList operationList)

calculationTextArea : Model  -> Html Msg
calculationTextArea model = Textfield.render Mdl [9] model.mdl
                              [ Textfield.label "Enter the formula"
                              , Textfield.textarea
                              , Textfield.rows 20
                              , Textfield.value (Maybe.withDefault "" model.calculationEditorModel.calculationFormulaText)
                              , Options.onInput (\s -> Internal (TextToCalculationTextArea s))
                              ]
                              []

resultMapNameText : Model  -> Html Msg
resultMapNameText model = Textfield.render Mdl [8] model.mdl
                              [ Textfield.label "Enter the result map name"
                              , Textfield.floatingLabel
                              , Textfield.value (Maybe.withDefault "" model.calculationEditorModel.resultMapName )
                              , Options.onInput (\s -> Internal (TextToResultNameText s))
                              ]
                              []

operationNameChoice : Model  -> Html Msg
operationNameChoice model = div []
  [ Toggles.radio Mdl [0] model.mdl
      [ Toggles.value (model.calculationEditorModel.operationMode == Union)
      , Toggles.group "operationName"
      , Toggles.ripple
      , Options.onToggle (Internal (ChangeOperationMode Union))
      ]
      [ text "Union" ]
  , Toggles.radio Mdl [1] model.mdl
      [ Toggles.value (model.calculationEditorModel.operationMode == Intersection)
      , Toggles.group "operationName"
      , Toggles.ripple
      , Options.onToggle (Internal (ChangeOperationMode Intersection))
      ]
      [ text "Intersection" ]
  ]
