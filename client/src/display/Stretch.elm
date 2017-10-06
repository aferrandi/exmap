module Stretch exposing (..)

import Html exposing (..)
import Html.Attributes
import Material.Options as Options exposing (css)

topDiv : List (Html msg) -> Html msg
topDiv = Options.div [
    css "display" "flex",
    css "flex-flow" "column",
    css "align-items" "stretch",
    css "width" "100vw",
    css "minHeight" "100vh",
    Options.attribute <| Html.Attributes.id "top"
    ]

stretchDiv : List (Html msg) -> Html msg
stretchDiv = Options.div [
    css "flex" "1 1 auto",
    css "overflow" "auto",
    Options.attribute <| Html.Attributes.id "stretch"
    ]

fixedDiv : List (Html msg) -> Html msg
fixedDiv = Options.div [
    css "flex" "0 0 10%",
    Options.attribute <| Html.Attributes.id "fixed"
    ]

