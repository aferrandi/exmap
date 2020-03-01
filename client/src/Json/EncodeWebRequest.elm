module Json.EncodeWebRequest exposing (..)

import Json.EncodeCalculation exposing (..)
import Json.EncodeProject exposing (..)
import Json.EncodeView exposing (..)
import Json.EncodeXMap exposing (..)
import Json.Encode exposing (..)
import Models.WebMessages exposing (..)

encodeWebRequest : WebRequest -> Value
encodeWebRequest ev =
    case ev of
        WRAllProjects ->
            object
                [ ( "type", string "allProjects" )
                ]
        WRSubscribeToProject pn ->
            object
                [ ( "type", string "subscribeToProject" )
                , ( "projectName", string pn )
                ]
        WRNewProject p ->
            object
                [ ( "type", string "newProject" )
                , ( "project", encodeProject p )
                ]
        WRUpdateProject p ->
            object
                [ ( "type", string "updateProject" )
                , ( "project", encodeProject p )
                ]
        WRLoadMap pn mn ->
            object
                [ ( "type", string "loadMap" )
                , ( "projectName", string pn )
                , ( "mapName", encodeXmapName mn )
                ]
        WRAddMap pn m ->
            object
                [ ( "type", string "addMap" )
                , ( "projectName", string pn )
                , ( "map", encodeXNamedMap m )
                ]
        WRUpdateMap pn m ->
            object
                [ ( "type", string "updateMap" )
                , ( "projectName", string pn )
                , ( "map", encodeXNamedMap m )
                ]
        WRSubscribeToView pn vn ->
            object
                [ ( "type", string "subscribeToView" )
                , ( "projectName", string pn )
                , ( "viewName", string vn )
                ]
        WRUnsubscribeFromView pn vn ->
            object
                [ ( "type", string "unsubscribeFromView" )
                , ( "projectName", string pn )
                , ( "viewName", string vn )
                ]
        WRMapsInProject pn ->
            object
                [ ( "type", string "mapsInProject" )
                , ( "projectName", string pn )
                ]
        WRLoadView pn vn ->
            object
                [ ( "type", string "loadView" )
                , ( "projectName", string pn )
                , ( "viewName", string vn )
                ]
        WRAddView pn v ->
            object
                [ ( "type", string "addView" )
                , ( "projectName", string pn )
                , ( "view", encodeView v )
                ]
        WRUpdateView pn v ->
            object
                [ ( "type", string "updateView" )
                , ( "projectName", string pn )
                , ( "view", encodeView v )
                ]
        WRLoadCalculation pn cn ->
            object
                [ ( "type", string "loadCalculation" )
                , ( "projectName", string pn )
                , ( "calculationName", string cn )
                ]
        WRAddCalculation pn cs ->
            object
                [ ( "type", string "addCalculation" )
                , ( "projectName", string pn )
                , ( "calculationSource", encodeCalculationSource cs )
                ]
        WRUpdateCalculation pn cs ->
            object
                [ ( "type", string "updateCalculation" )
                , ( "projectName", string pn )
                , ( "calculationSource", encodeCalculationSource cs )
                ]
        WRFunctions ->
            object
                [ ( "type", string "functions" )
                ]