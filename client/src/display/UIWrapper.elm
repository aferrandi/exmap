module UIWrapper exposing (..)

import Html        exposing (Html, text, div)
import Html.Events exposing (onClick)
import Material.Options as Options exposing (css)
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Color as Color
import Material.Typography as Typo
import Material.Toggles as Toggles
import Material.Textfield as Textfield
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

buttonNoClick : Model -> List Int -> String -> List (Button.Property Msg) -> Html Msg
buttonNoClick model indexes txt props = Button.render Mdl indexes model.mdl
                                              (
                                                  [ Button.raised
                                                  , Button.colored
                                                  ]
                                              ++ props)
                                              [ text txt]

buttonClick : Model -> List Int -> String -> Msg -> Html Msg
buttonClick model indexes txt msg = buttonNoClick model indexes txt [ Options.onClick msg ]

buttonMaybe : Model -> List Int -> String -> Maybe Msg -> Html Msg
buttonMaybe model indexes txt mmsg =
    let msgProp = case mmsg of
                        Just msg -> [ Options.onClick msg ]
                        Nothing -> []
    in buttonNoClick model indexes txt msgProp

scrollableTableStyle : Int -> List (Options.Property c Msg)
scrollableTableStyle h = [
        heightInView h,
        Options.css "overflow-y" "auto",
        Options.css "display" "block",
        Options.css "float" "left"
       ]

bold : List (Options.Property c Msg)
bold = [Options.css "font-weight" "1000"]


heightInView : Int -> Options.Property c Msg
heightInView h = Options.css "height" (toString h ++ "vh")


titleWithIcon : String -> String -> Color.Hue -> Html Msg
titleWithIcon s i c = Options.styled Html.label [ Typo.display1]
                        [ Icon.view i [Color.text (pastel c), Icon.size36], text s]

toggle : Model -> List Int -> String -> String -> Bool -> Msg -> Html Msg
toggle model idxs txt gp val msg = Toggles.radio Mdl idxs model.mdl
              [ Toggles.value val
              , Toggles.group gp
              , Toggles.ripple
              , Options.onToggle msg
              ]
              [ text txt]