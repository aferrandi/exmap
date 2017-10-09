module UIWrapper exposing (cell, buttonClick, buttonMaybe)

import Html        exposing (Html, text, div)
import Html.Events exposing (onClick)
import Material.Options as Options exposing (css)
import Material.Grid as Grid
import Material.Button as Button exposing (render)

import ProjectModel exposing (..)

cell : Int -> Int -> Int -> List (Html Msg) ->  Grid.Cell Msg
cell tablet desktop phone  = Grid.cell [ Grid.size Grid.Tablet tablet, Grid.size Grid.Desktop desktop, Grid.size Grid.Phone phone, Grid.stretch]

buttonNoClick : Model -> Int -> String -> List (Button.Property Msg) -> Html Msg
buttonNoClick model index txt props = Button.render Mdl [index] model.mdl
                                              (
                                                  [ Button.raised
                                                  , Button.colored
                                                  ]
                                              ++ props)
                                              [ text txt]

buttonClick : Model -> Int -> String -> Msg -> Html Msg
buttonClick model index txt msg = buttonNoClick model index txt [ Options.onClick msg ]

buttonMaybe : Model -> Int -> String -> Maybe Msg -> Html Msg
buttonMaybe model index txt mmsg =
    let msgProp = case mmsg of
                    Just msg -> [ Options.onClick msg ]
                    Nothing -> []
    in buttonNoClick model index txt msgProp
