module WebMessages exposing (..)

import XMapTypes exposing (..)
import Views exposing (..)
import Project exposing (..)

type WebRequest =
        WRAllProjects
        | WRSubscribeToProject ProjectName
        | WRNewProject Project
        | WRUpdateProject Project
        | WRLoadMaps ProjectName (List XMapName)
        | WRStoreMap ProjectName XNamedMap
        | WRSubscribeToView ProjectName ViewName
        | WRUnsubscribeFromView ProjectName ViewName

type WebEvent =
        WEAllProjects AllProjects
        | WEViewChanged ProjectName ViewName (List XNamedMap)
        | WEProjectContent Project
        | WEProjectStored ProjectName
        | WEMapsLoaded ProjectName (List XNamedMap)
        | WEMapStored ProjectName XMapName
        | WEUnsubscribedFromView ProjectName ViewName
        | WEViewStatus ProjectName View (List XNamedMap)
        | WEInfo String
        | WEError Error