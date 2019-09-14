module ViewsUI exposing (viewViews)

import Html exposing (Html, div, text)
import InternalMessages exposing (..)
import List.Extra as ListX

import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import MdlIndexes exposing (makeIndex, viewsUIIdx)
import ProjectModel exposing (..)
import UIWrapper exposing (..)
import ViewUI exposing (..)
import Views exposing (ViewName)


viewViews : Model -> ProjectModel -> Html Msg
viewViews model pm =
    let title =
            case model.currentView of
                Just viewName -> "View " ++ viewName
                Nothing -> "Views"
    in
    div []
        [ titleWithIcon title "view_comfy"  "Blue"
        , LayoutGrid.view [ heightInView 70 ]
            [ LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span2Desktop, LayoutGrid.span1Phone] [ viewAllViewsList model pm ]
            , LayoutGrid.cell [LayoutGrid.span6Tablet, LayoutGrid.span10Desktop, LayoutGrid.span3Phone] [ viewCurrentView model pm ]
            ]
        ]

viewAllViewsList : Model -> ProjectModel -> Html Msg
viewAllViewsList model pm =
    let
        sendOpenView index = sendListMsg (\vn -> Internal (OpenView vn)) pm.project.viewNames index
        viewViewName vn =
            Lists.li []
                [
                    Lists.graphicIcon [] "view_comfy",
                    text vn
                ]
    in
    Lists.ul Mdc (makeIndex viewsUIIdx 1) model.mdc
    [ Lists.onSelectListItem sendOpenView ] --(scrollableListStyle 65)
    (List.map viewViewName pm.project.viewNames)

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