module View where

import XMapTypes

newtype Label = Label String

data ViewItem = MapItem XMapName|
                LabelItem Label

-- horizontal on the screen
newtype ViewRow = ViewRow [ViewItem]

-- vertical on the screen
newtype View = View [ViewRow]

