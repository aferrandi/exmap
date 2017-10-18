module CalculationEditor exposing (viewCalculationEditor)

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
import List.Extra as ListX exposing (transpose)

import Project exposing (..)
import XMapTypes exposing(..)
import Views exposing (..)
import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ViewUI exposing (..)
import UIWrapper exposing (..)
import Stretch exposing (..)
import MapsExtraction exposing (xmapNameToString)


viewCalculationEditor : Model -> ProjectModel -> Html Msg
viewCalculationEditor model pm = Grid.grid []
                                [ cell 2 2 1 [ Color.background lighterGrey]  [mapsInProjectList model]
                                , cell 4 8 2 [] [ stretchDiv [calculationTextArea model ] ]
                                , cell 2 2 1 [ Color.background lighterGrey]  [functionsList model]
                                 ]

mapsInProjectList : Model -> Html Msg
mapsInProjectList model  =
    let listItem mn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Internal (AddMapToCalculation mn)) ]
                               [ Lists.avatarIcon "list" [], text (xmapNameToString mn) ]
                           ]
    in Lists.ul [] (List.map listItem (model.calculationEditorModel.mapsInProject))

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
        functions = Maybe.withDefault { applications = [], operations= [] }  model.calculationEditorModel.functions
        applicationList = List.map applicationListItem functions.applications
        operationList = List.map operationListItem functions.operations
    in Lists.ul [] ( List.append applicationList operationList)



calculationTextArea : Model  -> Html Msg
calculationTextArea model = Textfield.render Mdl [9] model.mdl
                              [ Textfield.label "Enter the formula"
                              , Textfield.floatingLabel
                              , Textfield.textarea
                              , Textfield.rows 20
                              , Textfield.value (Maybe.withDefault "" model.calculationEditorModel.calculationFormulaText)
                              , Options.onInput (\s -> Internal (TextToCalculationTextArea s))
                              ]
                              []
