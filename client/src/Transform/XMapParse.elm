module Transform.XMapParse exposing (textToMap)

import Dict as Dict
import Iso8601
import List.Extra as ListX exposing (break, find, transpose)
import Types.XMapTypes exposing (..)


textToMap : XMapType -> String -> Result String XMap
textToMap t s =
    textToMatrix s |> transpose |> matrixToMap t


toBool : String -> Result String Bool
toBool b =
    case b of
        "true" -> Ok True
        "True" -> Ok True
        "false" -> Ok False
        "False" -> Ok False
        _ -> Err (b ++ " is not a bool")


textToMatrix : String -> List (List String)
textToMatrix s =
    let
        textToRow r = String.words (String.trim r)
    in
        String.lines (String.trim s) |> List.map textToRow


matrixToMap : XMapType -> List (List String) -> Result String XMap
matrixToMap t ll =
    let
        compose = List.foldr (Result.map2 (::)) (Ok [])
        vs = Maybe.withDefault [] (ListX.last ll)
        ks = Maybe.withDefault [] (List.head ll)
        toDict = Result.map (\ovs -> ListX.zip ks ovs |> Dict.fromList |> MapValue)
        toFloat v = String.toFloat v |> Result.fromMaybe (v++" is not a float")
        toInt v = String.toInt v |> Result.fromMaybe (v++" is not an int")
        toDate v = Iso8601.toTime v |> Result.mapError (\e -> v++" is not a date")
    in
        case t of
            TypeDouble -> List.map toFloat vs |> compose |> toDict |> Result.map XMapDouble
            TypeInt -> List.map toInt vs |> compose |> toDict |> Result.map XMapInt
            TypeString -> Ok vs |> toDict |> Result.map XMapString
            TypeBool -> List.map toBool vs |> compose |> toDict |> Result.map XMapBool
            TypeDate -> List.map toDate vs |> compose |> toDict |> Result.map XMapDate
