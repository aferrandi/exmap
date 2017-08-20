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
    WEViewChanged XNamedMap
    | WEProjectContent Project
    | WEProjectStored ProjectName
    | WEMapStored XMapName
    | WEViewContent ViewName XNamedMap
    | WESubscribedToView ProjectName ViewName
    | WEUnsubscribedFromView ProjectName ViewName
    | WEError Error

