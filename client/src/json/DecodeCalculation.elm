module DecodeCalculation exposing (..)

import Calculation exposing (..)
import DecodeXMap exposing (xmapNameDecoder)
import Dict exposing (Dict, fromList, get)
import EnumToString exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

parameterTypeDecoder : Decoder ParameterType
parameterTypeDecoder =
    let
        decodeFromType t =
            case t of
                "double" -> succeed ParameterDouble
                "int" -> succeed ParameterInt
                "string" -> succeed ParameterString
                "bool" -> succeed ParameterBool
                "any" ->succeed ParameterAny
                otherwise ->
                    fail ("parameter type " ++ t ++ " not recognized")
    in
        string |> andThen decodeFromType


operationModeDecoder : Decoder OperationMode
operationModeDecoder =
    let
        m = fromList [ ( "Union", Union ), ( "Intersection", Intersection ) ]
    in
        string |> andThen (stringToEnum m)


calculationSourceDecoder : Decoder CalculationSource
calculationSourceDecoder =
    map4 CalculationSource
        (field "calculationName" string)
        (field "resultName" xmapNameDecoder)
        (field "formulaText" string)
        (field "operationMode" operationModeDecoder)


operationTypeDecoder : Decoder OperationType
operationTypeDecoder =
    map3 OperationType
        (field "name" string)
        (field "parametersTypes" (list parameterTypeDecoder))
        (field "returnType" parameterTypeDecoder)


functionsDecoder : Decoder Functions
functionsDecoder =
    map Functions
        (field "operationTypes" (list operationTypeDecoder))
