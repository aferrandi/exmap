module ProjectUI exposing (viewProject)

import Html        exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Scheme
import Material.Tabs as Tabs
import Material.Icon as Icon
import Material.List as Lists
import Material.Button as Button
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Options as Options exposing (css)
import List.Extra exposing (getAt)

import ProjectModel exposing (..)
import WebMessages exposing (WebRequest(..))
import ViewUI exposing (..)

viewProject : Model -> ProjectModel -> Html Msg
viewProject model pm = grid [ ]
                            [ cell [ size Tablet 2, size Desktop 3, size Phone 1 ]
                                [ viewAllViews model pm ]
                            , cell [ size Tablet 6, size Desktop 8, size Phone 3 ]
                                [ viewViewTabs model pm ]
                        ]


viewAllViews : Model -> ProjectModel -> Html Msg
viewAllViews model pm = let viewViewName vn = Lists.li []
                                                   [ Lists.content
                                                       [ Options.attribute <| Html.Events.onClick (Send (WRSubscribeToView pm.project.projectName vn)) ]
                                                       [ text vn ]
                                                   ]
                         in Lists.ul [] (List.map viewViewName pm.project.viewNames)



viewTabHeader : ViewModel -> Tabs.Label Msg
viewTabHeader vm = Tabs.label
               [ Options.center ]
               [ Icon.i "info_outline"
               , Options.span [ css "width" "4px" ] []
               , text vm.view.viewName
               ]


viewViewTabs :  Model -> ProjectModel -> Html Msg
viewViewTabs model pm = Tabs.render Mdl [0] model.mdl
 [ Tabs.ripple
 , Tabs.onSelectTab SelectViewTab
 , Tabs.activeTab model.viewTab
 ]
 (List.map viewTabHeader pm.openViews)
 [ viewViewAt model pm
 ]

viewViewAt : Model -> ProjectModel -> Html Msg
viewViewAt model pm = case getAt model.viewTab pm.openViews of
                    Just vm -> viewView model pm vm
                    Nothing -> div [][]
