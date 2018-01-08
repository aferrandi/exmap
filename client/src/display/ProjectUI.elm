

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
import List.Extra as ListX

import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ViewsUI exposing (..)
import UIWrapper exposing (..)
import MapEditor exposing (..)
import ViewEditor exposing (..)
import CalculationEditor exposing (..)
import InternalMessages exposing (..)
import MdlIndexes exposing (..)

viewProject : Model -> ProjectModel -> Html Msg
viewProject model pm = div[] [
                viewProjectTabs model,
                viewProjectContent model pm
                ]

viewProjectTabs : Model -> Html Msg
viewProjectTabs model =
    let tab icon title =   Tabs.label
                           [ Options.center, Color.text <| pastel Color.Blue ]
                           [ Icon.i icon
                           , Options.span [ css "width" "4px" ] []
                           , text title

                           ]

    in Tabs.render Mdl [projectUIIdx, 1] model.mdl
                   [ Tabs.ripple
                   , Tabs.onSelectTab tabToMessage
                   , Tabs.activeTab (projectFormToTab model.currentProjectForm)
                   , Color.text <| pastel Color.Blue
                   ]
                   [
                     tab "view_comfy" "Views",
                     tab "layers" "Map Editor",
                     tab "view_module" "View Editor",
                     tab "functions" "Calculation Editor"
                   ]
                   []

tabArray = [ViewsForm, MapEditorForm, ViewEditorForm,CalculationEditorForm]

tabToProjectForm : Int -> Maybe ProjectFormType
tabToProjectForm tabI = ListX.getAt tabI tabArray

projectFormToTab : ProjectFormType -> Int
projectFormToTab pf = Maybe.withDefault 0 (ListX.elemIndex pf tabArray)

tabToMessage : Int -> Msg
tabToMessage tabI =
    let msg =  case tabToProjectForm tabI of
                    Just pf -> SwitchProjectViewTo pf
                    Nothing -> ShowMessage "tab value not recognized"
    in Internal msg


viewProjectContent : Model -> ProjectModel -> Html Msg
viewProjectContent model pm = case model.currentProjectForm of
    ViewsForm -> viewViews model pm
    MapEditorForm -> mapEditorView model pm
    ViewEditorForm  -> viewViewsEditor model pm
    CalculationEditorForm  -> viewCalculationsEditor model pm

white : Options.Property c m
white = Color.text Color.white

