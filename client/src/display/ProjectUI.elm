module ProjectUI exposing (viewProject)

import Html exposing (Html,text, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class, style)
import Material.Tabs as Tabs
import Material.Icon as Icon
import Material.Dialog as Dialog
import Material.List as Lists
import Material.Menu as Menu exposing (Item)
import Material.Grid as Grid exposing (Device(..))
import Material.Options as Options exposing (css)
import List.Extra exposing (getAt)

import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ViewUI exposing (..)
import MapEditor exposing (viewModel)
import UIWrapper exposing (..)

viewProject : Model -> ProjectModel -> Html Msg
viewProject model pm = Grid.grid [ Grid.noSpacing ]
                                [ cell 2 2 1 [ viewAllViews model pm ]
                                , cell 6 10 3 [ viewViewTabs model pm ]
                                 ]

viewAllViews : Model -> ProjectModel -> Html Msg
viewAllViews model pm = div [] [
    viewAllViewsMenu model pm,
    viewAllViewsList model pm,
    MapEditor.viewModel model pm]

viewAllViewsMenu :  Model -> ProjectModel -> Html Msg
viewAllViewsMenu model pm = Menu.render Mdl [mdlIdxViews] model.mdl
                              [ Menu.bottomLeft ]
                              [ Menu.item
                                  [ Dialog.openOn "click" ]
                                  [ text "Map Editor" ]
                              , Menu.item
                                  [ Dialog.openOn "click" ]
                                  [ text "View Editor" ]
                              ]



viewAllViewsList : Model -> ProjectModel -> Html Msg
viewAllViewsList model pm =
    let viewViewName vn = Lists.li []
                           [ Lists.content
                               [ Options.attribute <| Html.Events.onClick (Send (WRSubscribeToView pm.project.projectName vn)) ]
                               [ Lists.avatarIcon "view_comfy" [], text vn ]
                           ]
    in Lists.ul [] (List.map viewViewName pm.project.viewNames)


viewViewTabs :  Model -> ProjectModel -> Html Msg
viewViewTabs model pm = Tabs.render Mdl [mdlIdxViews] model.mdl
 [ Tabs.ripple
 , Tabs.onSelectTab (\i -> Internal (SelectViewTab i))
 , Tabs.activeTab model.viewTab
 ]
 (List.map viewTabHeader pm.openViews)
 [ viewViewAt model pm ]

viewTabHeader : ViewModel -> Tabs.Label Msg
viewTabHeader vm = Tabs.label
               [ Options.center]
               [ Icon.i "view_comfy"
               , Options.span [ css "width" "4px" ] []
               , text vm.view.viewName
               ]


viewViewAt : Model -> ProjectModel -> Html Msg
viewViewAt model pm = case getAt model.viewTab pm.openViews of
                    Just vm -> viewView model pm vm
                    Nothing -> div [][]
