module WebMessages exposing (..)

import XMapTypes exposing (..)
import Views exposing (..)
import Project exposing (..)

type WebRequest =
        WRLoadProject ProjectName
        | WRNewProject Project
        | WRUpdateProject Project
        | WRLoadMap ProjectName XMapName
        | WRStoreMap ProjectName XNamedMap
        | WRSubscribeToView ProjectName ViewName
        | WRUnsubscribeFromView ProjectName ViewName

type WebEvent =
        WEViewChanged ProjectName ViewName XNamedMap
        | WEProjectContent Project
        | WEProjectStored ProjectName
        | WEMapLoaded ProjectName XNamedMap
        | WEMapStored ProjectName XMapName
        | WEUnsubscribedFromView ProjectName ViewName
        | WEViewStatus ProjectName View (List XNamedMap)
        | WEInfo String
        | WEError Error