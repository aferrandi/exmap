module ViewsUI exposing (viewViews)

import Html exposing (Html,text, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class, style)
import Material.Tabs as Tabs
import Material.Icon as Icon
import Material.Dialog as Dialog
import Material.List as Lists
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Menu as Menu exposing (Item)
import Material.Grid as Grid exposing (Device(..))
import Material.Options as Options exposing (css)
import List.Extra as ListX

import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import InternalMessages exposing (..)
import ViewUI exposing (..)
import UIWrapper exposing (..)


viewViews : Model -> ProjectModel -> Html Msg
viewViews model pm =    let title = case model.currentView of
                                Just viewName -> "View " ++ viewName
                                Nothing ->  "Views"
                        in div[]
                        [
                            titleWithIcon title "view_comfy" Color.Blue,
                            Grid.grid [heightInView 70]
                                [ cell 2 2 1 [ Color.background lighterGrey]  [viewAllViewsList model pm]
                                , cell 6 10 3 [] [ viewCurrentView model pm ]
                                 ]
                        ]

viewAllViewsList : Model -> ProjectModel -> Html Msg
viewAllViewsList model pm =
    let viewViewName vn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Internal (OpenView vn))]
                               [ Lists.avatarIcon "view_comfy" [], text vn ]
                           ]
    in Lists.ul [Color.background (Color.color Color.Grey Color.S100)] (List.map viewViewName pm.project.viewNames)



viewCurrentView : Model -> ProjectModel -> Html Msg
viewCurrentView model pm =
    let currentViewWithName cvn = case ListX.find (\v -> v.view.viewName == cvn) pm.openViews of
                                      Just vm -> viewView model pm vm
                                      Nothing -> div [][]
    in case model.currentView of
         Just cvn -> currentViewWithName cvn
         Nothing -> div [][]
