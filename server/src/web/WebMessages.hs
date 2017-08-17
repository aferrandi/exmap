module WebMessages where

import XMapTypes
import Project
import View

data WebRequest =
    WRLoadProject ProjectName
    | WRStoreProject Project
    | WRLoadMap ProjectName XMapName
    | WRStoreMap ProjectName XNamedMap
    | WRSubscribeToView ProjectName ViewName
    | WRUnSubscribeFromView ProjectName ViewName

data WebEvent =
    WEViewChanged XNamedMap
    | WEProjectContent Project
    | WEProjectStored ProjectName
    | WEMapStored XMapName
    | WEMapContent XNamedMap
    | WESubscribedToView ProjectName ViewName
    | WEUnsubscribedToView ProjectName ViewName

