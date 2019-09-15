module Views exposing (..)

import XMapTypes exposing (..)

type alias ViewLabel =
    String

type ViewItem
    = MapItem XMapName
    | LabelItem ViewLabel

-- horizontal on the screen
type ViewRow
    = ViewRow (List ViewItem)


type alias ViewName =
    String

-- vertical on the screen
type alias View =
    { viewName : ViewName
    , rows : List ViewRow
    }
