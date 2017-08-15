module WebMessages where

import XMapTypes
import Project
import View

data WebRequest =
    WLoadProject ProjectName
    | WStoreProject Project
    | WLoadMap ProjectName XMapName
    | WStoreMap ProjectName XNamedMap
    | WSubscribeToView ProjectName ViewName
    | WUnSubscribeFromView ProjectName ViewName
