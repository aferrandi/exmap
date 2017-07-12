module View where

import qualified Data.Text as T
import XMapTypes

newtype Label = Label T.Text

data ViewItem = MapItem XMapName|
                LabelItem Label

-- horizontal on the screen
newtype ViewRow = ViewRow [ViewItem]

-- vertical on the screen
newtype View = View [ViewRow]

