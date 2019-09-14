module EncodeCalculation exposing (..)

import Calculation exposing (..)
import Dict exposing (..)
import EncodeXMap exposing (..)
import Json.Encode exposing (..)
import String exposing (join)


encodeCalculationSource : CalculationSource -> Value
encodeCalculationSource cs =
    object
        [ ( "calculationName", string cs.calculationName )
        , ( "resultName", encodeXmapName cs.resultName )
        , ( "formulaText", string cs.formulaText )
        , ( "operationMode", encodeOperationMode cs.operationMode )
        ]

encodeOperationMode : OperationMode -> Value
encodeOperationMode om =
    case om of
        Union -> string "Union"
        Intersection-> string "Intersection"
