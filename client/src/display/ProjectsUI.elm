module ProjectsUI exposing (viewProjects)

import Html        exposing (Html, text, div)
import Html.Events exposing (onClick)
import Material
import Material.Color as Color
import Material.Scheme
import Material.Menu as Menu exposing (Item)
import Material.Tabs as Tabs
import Material.List as Lists
import Material.Icon as Icon
import Material.Grid as Grid
import Material.Options as Options exposing (css)
import List.Extra exposing (getAt)

import Stretch exposing (..)
import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ProjectUI exposing (..)
import Project exposing (ProjectName, Error)

viewProjects : Model -> Html Msg
viewProjects model = Material.Scheme.topWithScheme Color.Green Color.Amber (viewProjectsContent model)

viewProjectsContent : Model -> Html Msg
viewProjectsContent model = topDiv [ Color.background Color.white, Color.text Color.primary][
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


viewAllProjects : Model -> Html Msg
viewAllProjects model = div [] [
    viewAllProjectsMenu model,
    viewAllProjectsList model]

viewAllProjectsList : Model -> Html Msg
viewAllProjectsList model = Lists.ul [] (List.map viewAllProjectsItem model.allProjects)

viewAllProjectsMenu :  Model -> Html Msg
viewAllProjectsMenu model = Menu.render Mdl [mdlIdxProjects] model.mdl
                              [ Menu.bottomLeft ]
                              [ Menu.item
                                  [ Menu.onSelect (Internal NewProject) ]
                                  [ text "New Project" ]
                              ]

viewAllProjectsItem : ProjectName -> Html Msg
viewAllProjectsItem pn = Lists.li []
                            [ Lists.content
                                [ Options.attribute <| Html.Events.onClick (Send (WRSubscribeToProject pn)) ]
                                [ Lists.avatarIcon "folder" [], text pn ]
                            ]


projectTabHeader : ProjectModel -> Tabs.Label Msg
projectTabHeader pm = Tabs.label
               [ Options.center ]
               [ Icon.i "folder"
               , Options.span [ css "width" "4px" ] []
               , text pm.project.projectName
               ]

viewProjectTabs : Model -> Html Msg
viewProjectTabs model = Tabs.render Mdl [mdlIdxProjects] model.mdl
                             [ Tabs.ripple
                             , Tabs.onSelectTab (\i -> Internal (SelectProjectTab i))
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
