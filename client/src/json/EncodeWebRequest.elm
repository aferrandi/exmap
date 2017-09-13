module EncodeWebRequest exposing (..)

import Json.Encode exposing (..)
import EncodeProject exposing (..)
import WebMessages exposing (..)

encodeWebRequest : WebRequest -> Value
encodeWebRequest ev = case ev of
    WRAllProjects -> object
            [ ("type", string "allProjects")
            ]
    WRLoadProject pn ->  object
            [ ("type", string "loadProject")
            , ("projectName", string pn)
            ]
    WRNewProject p -> object
            [ ("type", string "newProject")
            , ("project", encodeProject p)
            ]
    WRUpdateProject p -> object
            [ ("type", string "updateProject")
            , ("project", encodeProject p)
            ]
    WRLoadMap pn mn -> object
            [ ("type", string "loadMap")
            , ("projectName", string pn)
            , ("mapName", encodeXmapName mn)
            ]
    WRStoreMap pn m -> object
            [ ("type", string "storeMap")
            , ("projectName", string pn)
            , ("map", encodeXNamedMap m)
            ]
    WRSubscribeToView pn vn -> object
            [ ("type", string "subscribeToView")
            , ("projectName", string pn)
            , ("viewName", string vn)
            ]
    WRUnsubscribeFromView pn vn -> object
            [ ("type", string "unsubscribeFromView")
            , ("projectName", string pn)
            , ("viewName", string vn)
            ]