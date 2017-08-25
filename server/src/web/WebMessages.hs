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

data WebEvent =
    WEViewChanged ProjectName ViewName XNamedMap
    | WEProjectContent Project
    | WEProjectStored ProjectName
    | WEMapStored ProjectName XMapName
    | WEUnsubscribedFromView ProjectName ViewName
    | WEViewStatus ProjectName View [XNamedMap]
    | WEError Error

