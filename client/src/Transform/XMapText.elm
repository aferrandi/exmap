module Transform.XMapText exposing (..)

import Dict as Dict
import List.Extra as ListX exposing (break, find, transpose)
import Transform.MapsExtraction exposing (..)
import Types.XMapTypes exposing (..)

mapToText : XMap -> String
mapToText m =
    let
        rowToText r = String.join " " r
    in
        mapToTransposedMatrix m |> List.map rowToText |> String.join "\n"

mapToMatrix : XMap -> List (List String)
mapToMatrix m =
    [ Dict.keys (mapValues m), Dict.values (mapValues m) ]


mapToTransposedMatrix : XMap -> List (List String)
mapToTransposedMatrix m =
    mapToMatrix m |> ListX.transpose
