module XMapText exposing (..)

import Dict as Dict
import List.Extra as ListX exposing (transpose, find, break)


import MapsExtraction exposing (..)
import XMapTypes exposing(..)

mapToMatrix : XMap -> List (List String)
mapToMatrix m = [Dict.keys (mapValues m), Dict.values (mapValues m)]

mapToTransposedMatrix : XMap -> List (List String)
mapToTransposedMatrix m = mapToMatrix m |> ListX.transpose

mapToText : XMap -> String
mapToText m = let rowToText r = String.join " " r
              in mapToTransposedMatrix m |> List.map rowToText |> String.join "\n"




mapType : XMap -> XMapType
mapType m = case m of
                XMapDouble _ -> TypeDouble
                XMapInt _ -> TypeInt
                XMapString _ -> TypeString
                XMapBool _ -> TypeBool
