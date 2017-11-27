module EncodeWebRequest exposing (..)

import Json.Encode exposing (..)
import EncodeXMap exposing (..)
import EncodeProject exposing (..)
import EncodeView exposing (..)
import EncodeCalculation exposing (..)

import WebMessages exposing (..)

encodeWebRequest : WebRequest -> Value
encodeWebRequest ev = case ev of
    WRAllProjects -> object
            [ ("type", string "allProjects")
            ]
    WRSubscribeToProject pn ->  object
            [ ("type", string "subscribeToProject")
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
    WRMapsInProject pn -> object
            [ ("type", string "mapsInProject")
            , ("projectName", string pn)
            ]
    WRLoadView pn vn -> object
            [ ("type", string "loadView")
            , ("projectName", string pn)
            , ("viewName", string vn )
            ]
    WRStoreView pn v -> object
            [ ("type", string "storeView")
            , ("projectName", string pn)
            , ("view", encodeView v)
            ]
    WRLoadCalculation pn cn -> object
            [ ("type", string "loadCalculation")
            , ("projectName", string pn)
            , ("calculationName", string cn)
            ]
    WRStoreCalculation pn cs -> object
           [ ("type", string "storeCalculation")
           , ("projectName", string pn)
           , ("calculationSource", encodeCalculationSource cs)
           ]
    WRFunctions -> object
            [ ("type", string "functions")
            ]
