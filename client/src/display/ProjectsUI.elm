module ProjectsUI exposing (..)

import Html        exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Options as Options exposing (css)

import ProjectModel exposing (..)

viewProjects : Model -> Html a
viewProjects model = grid []
                    [ cell [ size Tablet 2, size Desktop 3, size Phone 1 ]
                        [ h4 [] [text "Cell 1"]
                        ]
                    , cell [ size Tablet 6, size Desktop 8, size Phone 3 ]
                        [ h4 [] [text "Cell 2"]
                        , p [] [text "This cell is offset by 2"]
                        ]
                    ]
{-

viewProjectTabs :
Tabs.render Mdl [0] model.mdl
 [ Tabs.ripple
 , Tabs.onSelectTab SelectTab
 , Tabs.activeTab model.tab
 ]
 [ Tabs.label
     [ Options.center ]
     [ Icon.i "info_outline"
     , Options.span [ css "width" "4px" ] []
     , text "About tabs"
     ]
 , Tabs.label
     [ Options.center ]
     [ Icon.i "code"
     , Options.span [ css "width" "4px" ] []
     , text "Example"
     ]
 ]
 [ case model.tab of
     0 -> aboutTab
     _ -> exampleTab
 ]


 div
    []
    [ Lists.ul []
        [ Lists.li []
            [ Lists.content
                [ Options.attribute <| Html.Events.onClick (Click "Elm") ]
                [ text "Elm" ]
            ]
        , Lists.li []
            [ Lists.content
                [ Options.attribute <| Html.Events.onClick (Click "F#") ]
                [ text "F#" ]
            ]
        , Lists.li []
            [ Lists.content
                [ Options.attribute <| Html.Events.onClick (Click "Lisp") ]
                [ text "Lisp" ]
            ]
        ]
    , p []
        [ text <| "Try clicking a list item above. " ++
            if model.str /= "" then
              "You chose '" ++ model.str ++ "'."
            else
              ""
        ]
    ]

-}