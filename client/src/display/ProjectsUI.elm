module ProjectsUI exposing (viewProjects)

import Html        exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Color as Color
import Material.Scheme
import Material.Tabs as Tabs
import Material.List as Lists
import Material.Icon as Icon
import Material.Button as Button
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Material.Options as Options exposing (css, cs)
import Dict as Dict exposing (..)
import List.Extra exposing (getAt)

import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ProjectUI exposing (..)
import Project exposing (ProjectName, Error)

viewProjects : Model -> Html Msg
viewProjects model = Material.Scheme.topWithScheme Color.Green Color.LightGreen (viewProjectsContent model)

viewProjectsContent : Model -> Html Msg
viewProjectsContent model = topDiv [
                               stretchDiv [
                                   Grid.grid [ Grid.noSpacing]
                                      [ Grid.cell [ Grid.size Grid.Tablet 2, Grid.size Grid.Desktop 2, Grid.size Grid.Phone 1, Grid.stretch]
                                          [ viewAllProjects model ]
                                      , Grid.cell [ Grid.size Grid.Tablet 6, Grid.size Grid.Desktop 10, Grid.size Grid.Phone 3, Grid.stretch]
                                          [ viewProjectTabs model ]
                                  ]
                              ]
                              , fixedDiv [viewMessages model]
                          ]


topDiv : List (Html msg) -> Html msg
topDiv = Options.div [
    css "display" "flex",
    css "flex-flow" "column",
    css "align-items" "stretch",
    css "width" "100vw",
    css "minHeight" "100vh",
    Options.attribute <| Html.Attributes.id "top"
    ]

stretchDiv : List (Html msg) -> Html msg
stretchDiv = Options.div [
    css "flex" "1 1 auto",
    css "overflow" "auto",
    Options.attribute <| Html.Attributes.id "stretch"
    ]

fixedDiv : List (Html msg) -> Html msg
fixedDiv = Options.div [
    css "flex" "0 0 10%",
    Options.attribute <| Html.Attributes.id "fixed"
    ]


viewAllProjectsItem : ProjectName -> Html Msg
viewAllProjectsItem pn = Lists.li [Color.background (Color.color Color.Grey Color.S50)]
                            [ Lists.content
                                [ Options.attribute <| Html.Events.onClick (Send (WRSubscribeToProject pn)) ]
                                [ Lists.avatarIcon "folder" [], text pn ]
                            ]

viewAllProjects : Model -> Html Msg
viewAllProjects model = Lists.ul [] (List.map viewAllProjectsItem model.allProjects)


projectTabHeader : ProjectModel -> Tabs.Label Msg
projectTabHeader pm = Tabs.label
               [ Options.center ]
               [ Icon.i "folder"
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
viewProjectAt model = case getAt model.projectTab model.openProjects of
                    Just pm -> viewProject model pm
                    Nothing -> div [][]


viewMessagesItem : Error -> Html Msg
viewMessagesItem msg = Lists.li []
                          [ Lists.icon "inbox" []
                          , text msg
                          ]

viewMessages : Model -> Html Msg
viewMessages model = Lists.ul [] (List.map viewMessagesItem model.messages)
