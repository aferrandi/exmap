module ProjectsUI exposing (..)

import Html        exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Scheme
import Material.Tabs as Tabs
import Material.List as Lists
import Material.Icon as Icon
import Material.Button as Button
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Options as Options exposing (css)

import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))

viewProjects : Model -> Html Msg
viewProjects model = div [] [
                            div [class  "content"] [
                                grid [ ]
                                    [ cell [ size Tablet 2, size Desktop 3, size Phone 1 ]
                                        [ viewAllProjects model ]
                                    , cell [ size Tablet 6, size Desktop 8, size Phone 3 ]
                                        [ viewProjectTabs model ]
                                ]
                                ]
                            , footer [] [viewMessages model]
                        ]


viewAllProjects : Model -> Html Msg
viewAllProjects model = let viewProjectName pn = Lists.li []
                                                   [ Lists.content
                                                       [ Options.attribute <| Html.Events.onClick (Send (WRSubscribeToProject pn)) ]
                                                       [ text pn ]
                                                   ]
                        in Lists.ul [] (List.map viewProjectName model.allProjects)


viewProjectTabs : Model -> Html Msg
viewProjectTabs model = Tabs.render Mdl [0] model.mdl
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
 [ showProject model.tab
 ]

showProject : Int -> Html a
showProject index = div [][]

viewMessages : Model -> Html sg
viewMessages model = let viewMessage msg = Lists.li []
                                                   [ Lists.icon "inbox" []
                                                   , text msg
                                                   ]
                     in Lists.ul [] (List.map viewMessage model.messages)
