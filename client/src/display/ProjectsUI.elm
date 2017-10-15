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
import Material.Dialog as Dialog
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import List.Extra exposing (getAt)

import Stretch exposing (..)
import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ProjectUI exposing (..)
import Project exposing (ProjectName, Error)
import UIWrapper exposing (..)

viewProjects : Model -> Html Msg
viewProjects model =
    topDiv []
            [
                    stretchDiv [
                        Grid.grid []
                           [ cell 2 2 1 [Color.background lightGrey] [viewAllProjectsList model]
                           , cell 6 10 3 [] [ viewProjectTabs model ]
                       ]
                   ]
                   , fixedDiv [viewMessages model]
               ]

viewAllProjectsList : Model -> Html Msg
viewAllProjectsList model = Lists.ul [] (List.map viewAllProjectsItem model.allProjects)



viewAllProjectsItem : ProjectName -> Html Msg
viewAllProjectsItem pn = Lists.li []
                            [ Lists.content
                                [ Options.attribute <| Html.Events.onClick (Send (WRSubscribeToProject pn)) ]
                                [ Lists.avatarIcon "folder" [], text pn ]
                            ]

viewProjectTabs : Model -> Html Msg
viewProjectTabs model = Tabs.render Mdl [mdlIdxProjects] model.mdl
                             [ Tabs.ripple
                             , Tabs.onSelectTab (\i -> Internal (SelectProjectTab i))
                             , Tabs.activeTab model.projectTab
                             ]
                             (List.map projectTabHeader model.openProjects)
                         [ viewProjectAt model]

projectTabHeader : ProjectModel -> Tabs.Label Msg
projectTabHeader pm = Tabs.label
               [ Options.center]
               [ Icon.i "folder"
               , Options.span [ css "width" "4px" ] []
               , text pm.project.projectName
               ]



viewProjectAt : Model -> Html Msg
viewProjectAt model = case currentOpenProject model of
                    Just pm -> viewProject model pm
                    Nothing -> div [][]


viewMessagesItem : Error -> Html Msg
viewMessagesItem msg = Lists.li []
                          [ Lists.icon "inbox" []
                          , text msg
                          ]

viewMessages : Model -> Html Msg
viewMessages model = Lists.ul [Elevation.e4] (List.map viewMessagesItem model.messages)
