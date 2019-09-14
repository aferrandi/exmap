module ProjectUI exposing (viewProject)

import CalculationEditor exposing (..)
import Html exposing (Html, div, text)
import InternalMessages exposing (..)
import List.Extra as ListX
import MapEditor exposing (..)

import Material.Options as Options
import Material.TabBar as TabBar
import MdlIndexes exposing (..)
import ProjectModel exposing (..)
import ViewEditor exposing (..)
import ViewsUI exposing (..)


viewProject : Model -> ProjectModel -> Html Msg
viewProject model pm =
    div []
        [ viewProjectTabs model
        , viewProjectContent model pm
        ]

tabArray = [ ViewsForm, MapEditorForm, ViewEditorForm, CalculationEditorForm ]


-- Options.center, Color.text <| pastel Color.Blue
-- TabBar.ripple
viewProjectTabs : Model -> Html Msg
viewProjectTabs model =
    let tab icon title id =
            TabBar.tab
                [ TabBar.icon icon
                , Options.css "width" "4px"
                , Options.onClick (Internal (SwitchProjectViewTo id))
                ]
                [text title]
    in
    TabBar.view Mdc
        (makeIndex projectUIIdx 1)
        model.mdc
        [
          TabBar.activeTab (projectFormToTab model.currentProjectForm)
        --, TabBar.text <| pastel Color.Blue
        ]
        [ tab "view_comfy" "Views" ViewsForm
        , tab "layers" "Map Editor" MapEditorForm
        , tab "view_module" "View Editor" ViewEditorForm
        , tab "functions" "Calculation Editor" CalculationEditorForm
        ]

tabToProjectForm : Int -> Maybe ProjectFormType
tabToProjectForm tabI =
    ListX.getAt tabI tabArray


projectFormToTab : ProjectFormType -> Int
projectFormToTab pf =
    Maybe.withDefault 0 (ListX.elemIndex pf tabArray)


viewProjectContent : Model -> ProjectModel -> Html Msg
viewProjectContent model pm =
    case model.currentProjectForm of
        ViewsForm -> viewViews model pm
        MapEditorForm -> mapEditorView model pm
        ViewEditorForm -> viewViewsEditor model pm
        CalculationEditorForm -> viewCalculationsEditor model pm
