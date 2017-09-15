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


viewViewTabs :  Model -> ProjectModel -> Html Msg
viewViewTabs model pm = Tabs.render Mdl [0] model.mdl
 [ Tabs.ripple
 , Tabs.onSelectTab SelectViewTab
 , Tabs.activeTab model.viewTab
 ]
 [ Tabs.label
     [ Options.center ]
     [ Icon.i "info_outline"
     , Options.span [ css "width" "4px" ] []
     , text "About tabs"
     ]
 , Tabs.label
     [ Options.center ]
     [ Icon.i "code"
     , Options.span [ css "width" "4px" ] []
     , text "Example"
     ]
 ]
 [ showView model.viewTab
 ]

showView : Int -> Html a
showView index = div [][]
