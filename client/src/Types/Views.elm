module Types.Views exposing (..)

import Types.XMapTypes exposing (..)

type alias ViewLabel = String

type ViewItem
    = MapItem XMapName
    | LabelItem ViewLabel

type alias ViewEditItemId = Int

-- vertical on the screen
type alias ViewEditItem =
    { id : ViewEditItemId
    , content : ViewItem
    }


type ViewRowHedersType = RowHasHeader | RowNoHeader

type alias ViewRow =
    { items : List ViewItem
    , headerType : ViewRowHedersType
    }

-- horizontal on the screen
type alias ViewEditRow =
    { items : List ViewEditItem
    , headerType : ViewRowHedersType
    }

type alias ViewName = String

-- vertical on the screen
type alias ViewEdit =
    { viewName : ViewName
    , rows : List ViewEditRow
    }

-- vertical on the screen
type alias View =
    { viewName : ViewName
    , rows : List ViewRow
    }

