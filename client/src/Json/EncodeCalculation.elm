module Json.EncodeCalculation exposing (..)

import Types.Calculation exposing (..)
import Json.EncodeXMap exposing (..)
import Json.Encode exposing (..)


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
