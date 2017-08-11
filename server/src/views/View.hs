module View where

import qualified Data.Text as T
import XMapTypes

newtype ViewLabel = ViewLabel T.Text
    deriving (Show, Eq)

data ViewItem = MapItem XMapName|
                LabelItem ViewLabel
    deriving (Show, Eq)

-- horizontal on the screen
newtype ViewRow = ViewRow [ViewItem]
    deriving (Show, Eq)

newtype ViewName = ViewName T.Text
    deriving (Show, Eq)

-- vertical on the screen
data View = View {
    viewName :: ViewName,
    rows :: [ViewRow]
    }
    deriving (Show, Eq)


