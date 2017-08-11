module Messages where

import XMapTypes
import Project
import View

data Request =
    LoadProject ProjectName
    | StoreProject Project
    | LoadMap ProjectName XMapName
    | StoreMap ProjectName XNamedMap
    | SubscribeToView ProjectName ViewName
    | UnSubscribeFromView ProjectName ViewName
