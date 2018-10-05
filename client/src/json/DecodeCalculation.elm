module DecodeCalculation exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Dict exposing (Dict, get, fromList)

import DecodeXMap exposing (xmapNameDecoder)
import Calculation exposing (..)
import EnumToString exposing (..)

parameterTypeDecoder : Decoder ParameterType
parameterTypeDecoder =
    let decodeFromType t = case t of
                            "double" -> succeed ParameterDouble
                            "int" -> succeed ParameterInt
                            "string" -> succeed ParameterString
                            "bool" -> succeed ParameterBool
                            "any" -> succeed ParameterAny
                            otherwise -> fail ("parameter type " ++ t ++ " not recognized")
    in string |> andThen decodeFromType

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

operationTypeDecoder : Decoder OperationType
operationTypeDecoder = decode OperationType
                   |> required "name" string
                   |> required "parametersTypes" (list parameterTypeDecoder)
                   |> required "returnType" parameterTypeDecoder

functionsDecoder : Decoder Functions
functionsDecoder = decode Functions
                   |> required "operationTypes" (list operationTypeDecoder)
