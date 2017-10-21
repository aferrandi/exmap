

module ProjectUI exposing (viewProject)

import Html exposing (Html,text, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class, style)
import Material.Tabs as Tabs
import Material.Icon as Icon
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Menu as Menu exposing (Item)
import Material.Grid as Grid exposing (Device(..))
import Material.Options as Options exposing (css)
import List.Extra exposing (getAt)

import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ViewsUI exposing (..)
import UIWrapper exposing (..)
import Stretch exposing (..)
import MapEditor exposing (..)
import ViewEditor exposing (..)
import CalculationEditor exposing (..)

viewProject : Model -> ProjectModel -> Html Msg
viewProject model pm = Grid.grid [heightInView 75]
                                [ cell 7 10 3 [] [viewProjectContent model pm]
                                , cell 1 2 1 [] [ viewCards model pm ]
                                 ]

viewProjectContent : Model -> ProjectModel -> Html Msg
viewProjectContent model pm = case model.currentProjectView of
    ViewsView -> viewViews model pm
    MapEditorView -> mapEditorView model pm
    ViewEditorView -> viewViewEditor model pm
    CalculationEditorView -> viewCalculationsEditor model pm

viewCards :  Model -> ProjectModel -> Html Msg
viewCards model pm = stretchDiv [
    viewCard model "Views" "Live views" 0 Color.LightBlue ViewsView
    , viewCard model "Map Editor" "create and update maps" 1 Color.DeepOrange MapEditorView
    , viewCard model "View Editor" "create and update views" 2 Color.Pink ViewEditorView
    , viewCard model "Calculation Editor" "create and update calculations" 3 Color.Green CalculationEditorView
    ]

white : Options.Property c m
white = Color.text Color.white

viewCard : Model -> String -> String -> Int -> Color.Hue -> ProjectViewType -> Html Msg
viewCard model title cardText i hue viewType =  Card.view
          [ Color.background (pastel hue)
          , css "width" "192px"
         , css "height" "20vh"
          ]
          [ Card.title [ ] [ Card.head [ white ] [ text title ] ]
           , Card.text [ white ] [ text cardText ]
          , Card.actions
          [ Card.border, css "vertical-align" "center", css "text-align" "right", white ]
              [ Button.render Mdl [ i ] model.mdl
                  [ Button.raised, Button.ripple, Options.onClick (Internal (SwitchProjectViewTo viewType)) ]
                    [ text "Switch to" ]
              ]
          ]

