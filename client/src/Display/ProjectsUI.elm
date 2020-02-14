module Display.ProjectsUI exposing (viewProjects)

import Html exposing (Html, div, text)
import Models.InternalMessages exposing (..)

import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Material.Options as Options
import Material.TopAppBar as TopAppBar
import Display.MdcIndexes exposing (..)
import Display.NameDialog exposing (nameDialog)
import Types.Project exposing (Error, ProjectName)
import Models.ProjectModel exposing (..)
import Display.ProjectUI exposing (..)
import Display.UIWrapper exposing (..)

title : Model -> String
title model =
    let
        titleWithProject = Maybe.map (\p -> ". Project " ++ p) model.currentProject
    in
        "EXMAP" ++ Maybe.withDefault "" titleWithProject

viewProjects : Model -> Html Msg
viewProjects model =
    div []
        [ LayoutGrid.view [ heightInView 90 ]
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
        div []
        [
            Html.text (title model),
            Lists.ul Mdc (makeIndex projectsUIIdx "lstAllPrj") model.mdc
                ([ Lists.onSelectListItem sendOpenProject ] ++ (scrollableListStyle 75))
                (List.map viewAllProjectsItem model.allProjects)
        ]

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
    Lists.ul Mdc (makeIndex projectsUIIdx "lstMsg") model.mdc  (scrollableListStyle 10) (List.map viewMessagesItem model.messages)

newProjectButton : Model -> Html Msg
newProjectButton model =
    let
        newProjectMessage = Internal (NewProjectWithName model.newProjectName)
        idxDialog = makeIndex projectsUIIdx "dlgNewPrj"
    in
        div []
        [
          nameDialog idxDialog model "Create and store project" (\s -> Internal (UpdateProjectName s)) newProjectMessage
        , buttonClick model (makeIndex projectsUIIdx "btnNewPrj") "Create and store project"  (Internal (ShowDialog idxDialog))
        ]
