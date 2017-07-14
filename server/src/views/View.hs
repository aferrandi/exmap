module View where

import qualified Data.Text as T
import XMapTypes

newtype Label = Label T.Text
    deriving (Show, Eq)

data ViewItem = MapItem XMapName|
                LabelItem Label
    deriving (Show, Eq)

-- horizontal on the screen
newtype ViewRow = ViewRow [ViewItem]
    deriving (Show, Eq)

-- vertical on the screen
newtype View = View [ViewRow]
    deriving (Show, Eq)
