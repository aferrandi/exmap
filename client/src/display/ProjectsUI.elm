module ProjectsUI exposing (viewProjects)

import Html exposing (Html, div, text)
import InternalMessages exposing (..)

import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Material.Options as Options
import Material.TextField as TextField
import Material.TopAppBar as TopAppBar
import MdlIndexes exposing (..)
import Project exposing (Error, ProjectName)
import ProjectModel exposing (..)
import ProjectUI exposing (..)
import UIWrapper exposing (..)
import WebMessages exposing (WebRequest(..))

topAppBar : Model -> Html Msg
topAppBar model  =
    TopAppBar.view Mdc
        (makeIndex projectsUIIdx 1)
        model.mdc
        []
        [ TopAppBar.section
            [TopAppBar.alignStart]
            [TopAppBar.title [] [ text (title model) ]]
        ]

title : Model -> String
title model =
    let
        titleWithProject =
            Maybe.map (\p -> ". Project " ++ p) model.currentProject
    in
        "EXMAP" ++ Maybe.withDefault "" titleWithProject


viewProjects : Model -> Html Msg
viewProjects model =
    div []
        [
         div [] [topAppBar model]
        ,  Options.styled div [ TopAppBar.fixedAdjust] [viewProjectsContent model ]
        ]


viewProjectsContent : Model -> Html Msg
viewProjectsContent model =
    div []
        [ LayoutGrid.view [ heightInView 80 ]
            [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span2Desktop, LayoutGrid.span1Phone] [ viewAllProjectsList model, newProjectButton model ]
            , LayoutGrid.cell [LayoutGrid.span6Tablet, LayoutGrid.span10Desktop, LayoutGrid.span3Phone] [ viewCurrentProject model ]
            ]
        , viewMessages model
        ]

viewAllProjectsList : Model -> Html Msg
viewAllProjectsList model =
    let
        sendOpenProject index = sendListMsg (\pn -> Internal (OpenProject pn)) model.allProjects index
    in
        Lists.ul Mdc (makeIndex projectsUIIdx 4) model.mdc
            [ Lists.onSelectListItem sendOpenProject ] -- (scrollableListStyle 65)
            (List.map viewAllProjectsItem model.allProjects)

viewAllProjectsItem : ProjectName -> Lists.ListItem Msg
viewAllProjectsItem pn =
    Lists.li []
        [
          Lists.graphicIcon [] "folder",
          text pn
        ]

viewCurrentProject : Model -> Html Msg
viewCurrentProject model =
    case currentProjectModel model of
        Just pm -> viewProject model pm
        Nothing -> div [] []


viewMessagesItem : Error -> Lists.ListItem Msg
viewMessagesItem msg =
    Lists.li []
        [ Lists.graphicIcon [] "inbox"
        , text msg
        ]

viewMessages : Model -> Html Msg
viewMessages model =
    Lists.ul Mdc (makeIndex projectsUIIdx 3) model.mdc [] (List.map viewMessagesItem model.messages)

newProjectButton : Model -> Html Msg
newProjectButton model =
    let
        newProjectMessage =
            Send
                (WRNewProject
                    { projectName = model.newProjectName
                    , calculations = []
                    , viewNames = []
                    , sources = []
                    }
                )
    in
        div []
            [ TextField.view Mdc
                (makeIndex projectsUIIdx 2)
                model.mdc
                [ TextField.label "New project name"
                -- , TextField.floatingLabel
                -- , TextField.text_
                , TextField.value model.newProjectName
                , Options.onInput (\s -> Internal (UpdateProjectName s))
                ]
                []
            , buttonClick model (makeIndex projectsUIIdx 2) "Create and store project" newProjectMessage
            ]
