module Transform.MapsExtractionTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Dict exposing (..)

import Transform.MapsExtraction as MapsExtraction exposing (..)
import Types.XMapTypes exposing (..)



suite : Test
suite = describe "The MapsExtraction module"
                [ describe "MapsExtraction.mapKeys" -- Nest as many descriptions as you like.
                    [ test "extracts double matrix keys" <|
                        \_ ->
                            XMapDouble (MapValue (Dict.fromList [("pane", 10), ("latte", 20)]))
                                |> MapsExtraction.mapKeys
                                |> Expect.equal ["latte", "pane"]
                    , test "extracts double matrix values as strings" <|
                        \_ ->
                            XMapDouble (MapValue (Dict.fromList [("pane", 10), ("latte", 20)]))
                                |> MapsExtraction.mapValues
                                |> Expect.equal (Dict.fromList [("pane", "10"), ("latte", "20")])
                    ]
                ]