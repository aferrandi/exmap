module UIWrapper exposing (..)

import Html        exposing (Html, text, div)
import Html.Events exposing (onClick)
import Material.Options as Options exposing (css)
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Color as Color
import Material.Typography as Typo
import Material.Button as Button exposing (render)

import ProjectModel exposing (..)

lightGrey : Color.Color
lightGrey = Color.color Color.Grey Color.S200

lighterGrey : Color.Color
lighterGrey = Color.color Color.Grey Color.S100

pastel : Color.Hue -> Color.Color
pastel hue = Color.color hue Color.S400

cell : Int -> Int -> Int -> List (Options.Style Msg) -> List (Html Msg) ->  Grid.Cell Msg
cell tablet desktop phone others =
    Grid.cell ([ Grid.size Grid.Tablet tablet, Grid.size Grid.Desktop desktop, Grid.size Grid.Phone phone, Grid.stretch] ++ others)

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

scrollableTableStyle : List (Options.Property c Msg)
scrollableTableStyle = [Options.css "height" "80vh", Options.css "overflow-y" "auto", Options.css "display" "block"]

heightInView : Int -> Options.Property c Msg
heightInView h = Options.css "height" (toString h ++ "vh")

title : String -> Html Msg
title s = Options.styled Html.p [ Typo.display1] [ text s ]

titleWithIcon : String -> String -> Color.Hue -> Html Msg
titleWithIcon s i c = Html.span [] [Icon.view i [Color.text (pastel c), Icon.size48], title s]
