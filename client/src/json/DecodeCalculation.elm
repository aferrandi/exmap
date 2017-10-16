module DecodeCalculation exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Dict exposing (Dict, get, fromList)

import DecodeXMap exposing (..)
import Calculation exposing (..)
import EnumToString exposing (..)

operationModeDecoder : Decoder OperationMode
operationModeDecoder =
    let m = fromList [("Union",Union), ("Intersection",Intersection)]
    in string |> andThen (stringToEnum m)

calculationSourceDecoder : Decoder CalculationSource
calculationSourceDecoder = decode CalculationSource
                   |> required "calculationName" string
                   |> required "resultName" xmapNameDecoder
                   |> required "formulaText" string
                   |> required "operationMode" operationModeDecoder