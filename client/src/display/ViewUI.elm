module ViewUI exposing (viewView)

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

viewView : Model -> ViewModel -> Html Msg
viewView model vm = div [][]