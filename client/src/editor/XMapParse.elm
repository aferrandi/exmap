module XMapParse exposing (textToMap)

import Dict as Dict
import List.Extra as ListX exposing (transpose, find, break)


import MapsExtraction exposing (..)
import XMapTypes exposing(..)

textToMap : XMapType -> String -> Result String XMap
textToMap t s = textToMatrix t s |> matrixToDict |> Result.map (dictToMap t)


toBool : String -> Result String Bool
toBool b = case b of
                "true" -> Ok True
                "false" -> Ok False
                _ -> Err (b ++ " is not a bool")

textToMatrix : String -> List (List String)
textToMatrix t s = let textToRow r = String.split " " r
                   in  String.split "\n" s |> List.map textToRow

textsToMapValues : XMapType -> List String-> Result String (List a)
textsToMapValues t l = let textToValue s = case t of
                                TypeDouble -> String.toFloat s
                                TypeInt -> String.toInt s
                                TypeString -> s
                                TypeBool -> toBool s
                           values = List.map textToValue l
                       in List.foldr (Result.map (::)) values

matrixToDict : XMapType -> List (List String) -> Result String (MapValue a)
matrixToDict t ll = let toMap vs = Dict.fromList (ListX.zip (Maybe.withDefault[] (List.head ll)) vs) |> MapValue
                   in if List.length ll == 2
                        then Result.map toMap (textsToMapValues t (Maybe.withDefault [] (ListX.last ll)))
                        else Result Err "list must contain 2 elements"

dictToMap : XMapType -> MapValue a -> XMap
dictToMap t m = case t of
                    TypeDouble -> XMapDouble m
                    TypeInt -> XMapInt m
                    TypeString -> XMapString m
                    TypeBool -> XMapBool m

