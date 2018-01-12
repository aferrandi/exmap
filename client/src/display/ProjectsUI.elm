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
import Material.Toggles as Toggles
import Material.Textfield as Textfield
import List.Extra as ListX

import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ProjectUI exposing (..)
import Project exposing (ProjectName, Error)
import UIWrapper exposing (..)
import Material.Layout as Layout
import InternalMessages exposing (..)
import MdlIndexes exposing (..)

layoutHeader : Model -> Html Msg
layoutHeader model =
    Layout.row
        [ Color.background <| pastel Color.Blue
        , Color.text <| Color.white
        ]
        [ Layout.title [] [ text (title model) ]
        , Layout.spacer
        , Layout.navigation [] []
        ]

title : Model -> String
title model = let titleWithProject = Maybe.map (\p -> ". Project " ++ p) model.currentProject
              in "EXMAP" ++ (Maybe.withDefault "" titleWithProject)


viewProjects : Model -> Html Msg
viewProjects model = Layout.render Mdl model.mdl
  [ Layout.fixedHeader
  ]
  { header = [ layoutHeader model ]
  , drawer = [  ]
  , tabs = ([ ], [ ])
  , main = [viewProjectsContent model |> Material.Scheme.topWithScheme Color.Grey Color.Red]
  }

viewProjectsContent : Model -> Html Msg
viewProjectsContent model =
            div [] [
                        Grid.grid [heightInView 80]
                           [ cell 2 2 1 [] [viewAllProjectsList model, newProjectButton model]
                           , cell 6 10 3 [] [ viewCurrentProject model ]
                   ]
                   , viewMessages model
               ]

viewAllProjectsList : Model -> Html Msg
viewAllProjectsList model = Lists.ul (scrollableListStyle 65) (List.map viewAllProjectsItem model.allProjects)

viewAllProjectsItem : ProjectName -> Html Msg
viewAllProjectsItem pn = Lists.li []
                            [ Lists.content
                                [ Options.attribute <| Html.Events.onClick (Internal (OpenProject pn)) ]
                                [ Lists.avatarIcon "folder" [], text pn ]
                            ]

viewCurrentProject : Model -> Html Msg
viewCurrentProject model = case  currentProjectModel model  of
                                Just pm -> viewProject model pm
                                Nothing -> div [][]

viewMessagesItem : Error -> Html Msg
viewMessagesItem msg = Lists.li []
                          [ Lists.icon "inbox" []
                          , text msg
                          ]

viewMessages : Model -> Html Msg
viewMessages model = Lists.ul [] (List.map viewMessagesItem model.messages)

newProjectButton : Model -> Html Msg
newProjectButton model =
    let newProjectMessage = Send (WRNewProject {
                                    projectName= model.newProjectName,
                                    calculations = [],
                                    viewNames = [],
                                    sources = []})
    in div []
        [ Textfield.render Mdl [projectsUIIdx, 1] model.mdl
                                             [ Textfield.label "New project name"
                                             , Textfield.floatingLabel
                                             , Textfield.text_
                                             , Textfield.value model.newProjectName
                                             , Options.onInput (\s -> Internal (UpdateProjectName s))
                                             ] []
        , buttonClick model [projectsUIIdx, 2] "Create and store project" newProjectMessage]