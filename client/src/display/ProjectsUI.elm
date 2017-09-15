module ProjectsUI exposing (viewProjects)

import Html        exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Scheme
import Material.Tabs as Tabs
import Material.List as Lists
import Material.Icon as Icon
import Material.Button as Button
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Material.Options as Options exposing (css)
import Dict as Dict exposing (..)

import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ProjectUI exposing (..)

viewProjects : Model -> Html Msg
viewProjects model = div [] [
                            div [class  "content"] [
                                Grid.grid [ ]
                                    [ Grid.cell [ Grid.size Grid.Tablet 2, Grid.size Grid.Desktop 3, Grid.size Grid.Phone 1 ]
                                        [ viewAllProjects model ]
                                    , Grid.cell [ Grid.size Grid.Tablet 6, Grid.size Grid.Desktop 8, Grid.size Grid.Phone 3 ]
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
viewProjectTabs model = let projectTab pm = Tabs.label
                                       [ Options.center ]
                                       [ Icon.i "info_outline"
                                       , Options.span [ css "width" "4px" ] []
                                       , text pm.project.projectName
                                       ]
                        in Tabs.render Mdl [0] model.mdl
                             [ Tabs.ripple
                             , Tabs.onSelectTab SelectProjectTab
                             , Tabs.activeTab model.projectTab
                             ]
                             (List.map projectTab (Dict.values model.openProjects))
                         [ showProject model.projectTab
                         ]

showProject : Int -> Html a
showProject index = div [][]

viewMessages : Model -> Html sg
viewMessages model = let viewMessage msg = Lists.li []
                                                   [ Lists.icon "inbox" []
                                                   , text msg
                                                   ]
                     in Lists.ul [] (List.map viewMessage model.messages)
