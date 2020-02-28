module Display.ViewsUI exposing (viewViews)

import Html exposing (Html, div, text)
import Material.Snackbar as Snackbar exposing (..)
import Models.InternalMessages exposing (..)
import List.Extra as ListX

import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Display.MdcIndexes exposing (makeIndex, viewsUIIdx)
import Models.ProjectModel exposing (..)
import Display.UIWrapper exposing (..)
import Display.ViewUI exposing (..)
import Types.Views exposing (ViewName)


viewViews : Model -> ProjectModel -> Html Msg
viewViews model pm =
        LayoutGrid.view [ heightInView model.ui.heights.viewProjects ]
            [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span2Desktop, LayoutGrid.span1Phone] [ viewAllViewsList model pm ]
            , LayoutGrid.cell [LayoutGrid.span6Tablet, LayoutGrid.span10Desktop, LayoutGrid.span3Phone] [ viewCurrentView model pm ]
            ]

title : Model -> String
title model =
    case model.currentView of
        Just viewName -> "View " ++ viewName
        Nothing -> "Views"

viewAllViewsList : Model -> ProjectModel -> Html Msg
viewAllViewsList model pm =
    let
        sendOpenView index = sendListMsg (\vn -> Internal (OpenView vn)) pm.project.viewNames index
        viewViewName vn = Lists.li []
                            [
                                Lists.graphicIcon [] "view_comfy",
                                text vn
                            ]
    in
        div []
        [
            titleWithIcon (title model) "view_comfy"  "Blue",
            Lists.ul Mdc (makeIndex viewsUIIdx "lstAllVew") model.mdc
            ((Lists.onSelectListItem sendOpenView) :: (scrollableListStyle model.ui.heights.viewAllViewsList))
            (List.map viewViewName pm.project.viewNames)
        ]

viewCurrentView : Model -> ProjectModel -> Html Msg
viewCurrentView model pm =
    let
        currentViewWithName cvn =
            case ListX.find (\v -> v.view.viewName == cvn) pm.openViews of
                Just vm -> viewView model pm vm
                Nothing -> div [] []
    in
    case model.currentView of
        Just cvn -> currentViewWithName cvn
        Nothing -> div [] []

