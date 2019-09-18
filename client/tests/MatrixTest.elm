module MatrixTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import List.Extra exposing (transpose)



suite : Test
suite = describe "The Matrix module"
                [ describe "Matrix.transpose" -- Nest as many descriptions as you like.
                    [ test "no effect on 1 element list" <|
                        \_ ->
                            let
                                singleton = [["s"]]
                            in
                                Expect.equal singleton (transpose singleton)

                    , test "reverses a known string" <|
                        \_ ->
                            [["s"], ["a"]]
                                |> transpose
                                |> Expect.equal [["s", "a"]]

                    ]
                ]