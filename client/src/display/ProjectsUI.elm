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
import Project exposing (ProjectName, Error)

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


viewAllProjectsItem : ProjectName -> Html Msg
viewAllProjectsItem pn = Lists.li []
                            [ Lists.content
                                [ Options.attribute <| Html.Events.onClick (Send (WRSubscribeToProject pn)) ]
                                [ text pn ]
                            ]

viewAllProjects : Model -> Html Msg
viewAllProjects model = Lists.ul [] (List.map viewAllProjectsItem model.allProjects)


getAt : List a -> Int -> Maybe a
getAt l i = List.head (List.drop i l)


projectTabHeader : ProjectModel -> Tabs.Label Msg
projectTabHeader pm = Tabs.label
               [ Options.center ]
               [ Icon.i "info_outline"
               , Options.span [ css "width" "4px" ] []
               , text pm.project.projectName
               ]

viewProjectTabs : Model -> Html Msg
viewProjectTabs model = Tabs.render Mdl [0] model.mdl
                             [ Tabs.ripple
                             , Tabs.onSelectTab SelectProjectTab
                             , Tabs.activeTab model.projectTab
                             ]
                             (List.map projectTabHeader model.openProjects)
                         [ viewProjectAt model]

viewProjectAt : Model -> Html Msg
viewProjectAt model = case getAt model.openProjects model.projectTab of
                    Just pm -> viewProject model pm
                    Nothing -> div [][]


viewMessagesItem : Error -> Html Msg
viewMessagesItem msg = Lists.li []
                          [ Lists.icon "inbox" []
                          , text msg
                          ]

viewMessages : Model -> Html Msg
viewMessages model = Lists.ul [] (List.map viewMessagesItem model.messages)
