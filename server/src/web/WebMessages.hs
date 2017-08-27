module WebMessages where

import XMapTypes
import Project
import View

data WebRequest =
    WRLoadProject ProjectName
    | WRNewProject Project
    | WRUpdateProject Project
    | WRLoadMap ProjectName XMapName
    | WRStoreMap ProjectName XNamedMap
    | WRSubscribeToView ProjectName ViewName
    | WRUnsubscribeFromView ProjectName ViewName
    deriving (Show, Eq)

data WebEvent =
    WEViewChanged ProjectName ViewName XNamedMap
    | WEProjectContent Project
    | WEProjectStored ProjectName
    | WEMapLoaded ProjectName XNamedMap
    | WEMapStored ProjectName XMapName
    | WEUnsubscribedFromView ProjectName ViewName
    | WEViewStatus ProjectName View [XNamedMap]
    | WEError Error
    deriving (Show, Eq)

