module View where

import qualified Data.Text as T
import XMapTypes

data ViewRowHeaderType = RowHasHeader | RowNoHeader
    deriving (Show, Eq, Read)

newtype ViewLabel = ViewLabel T.Text
    deriving (Show, Eq)

data ViewItem = MapItem XMapName|
                LabelItem ViewLabel
    deriving (Show, Eq)

-- horizontal on the screen
data ViewRow = ViewRow {
    items :: [ViewItem],
    headerType :: ViewRowHeaderType
    }
    deriving (Show, Eq)

newtype ViewName = ViewName T.Text
    deriving (Show, Eq, Ord)

-- vertical on the screen
data View = View {
    viewName :: ViewName,
    rows :: [ViewRow]
    }
    deriving (Show, Eq)


