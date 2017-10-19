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
import List.Extra exposing (getAt)

import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ViewUI exposing (..)
import UIWrapper exposing (..)
import Stretch exposing (..)


viewViews : Model -> ProjectModel -> Html Msg
viewViews model pm = div[]
                        [
                            titleWithIcon "Views" "view_comfy" Color.Blue,
                            Grid.grid [Options.css "height" "80vh"]
                                [ cell 2 2 1 [ Color.background lighterGrey]  [viewAllViewsList model pm]
                                , cell 6 10 3 [] [ stretchDiv [viewViewAt model pm] ]
                                 ]

                        ]


viewAllViewsList : Model -> ProjectModel -> Html Msg
viewAllViewsList model pm =
    let viewViewName vn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Send (WRSubscribeToView pm.project.projectName vn)) ]
                               [ Lists.avatarIcon "view_comfy" [], text vn ]
                           ]
    in Lists.ul [Color.background (Color.color Color.Grey Color.S100)] (List.map viewViewName pm.project.viewNames)



viewViewAt : Model -> ProjectModel -> Html Msg
viewViewAt model pm = case getAt model.viewTab pm.openViews of
                    Just vm -> viewView model pm vm
                    Nothing -> div [][]
