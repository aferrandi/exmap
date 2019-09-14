module UIWrapper exposing (..)

import Html exposing (Html, text)
import List.Extra as ListX
import Material.Button as Button

import Material.Icon as Icon
import Material.Options as Options
import Material.RadioButton as RadioButton
import Material.Typography as Typo
import ProjectModel exposing (..)


buttonNoClick : Model -> String -> String -> List (Button.Property Msg) -> Html Msg
buttonNoClick model index txt props =
    Button.view Mdc
        index
        model.mdc
        ([ Button.raised
         -- , Button.colored
         ]
            ++ props
        )
        [ text txt ]


buttonClick : Model -> String -> String -> Msg -> Html Msg
buttonClick model index txt msg =
    buttonNoClick model index txt [ Options.onClick msg ]


buttonMaybe : Model -> String -> String -> Maybe Msg -> Html Msg
buttonMaybe model index txt mmsg =
    let
        msgProp =
            case mmsg of
                Just msg ->
                    [ Options.onClick msg ]

                Nothing ->
                    []
    in
    buttonNoClick model index txt msgProp


scrollableTableStyle : Int -> List (Options.Property c Msg)
scrollableTableStyle h =
    [ heightInView h
    , Options.css "overflow-y" "auto"
    , Options.css "display" "block"
    , Options.css "float" "left"
    ]


scrollableListStyle : Int -> List (Options.Property c Msg)
scrollableListStyle h =
    [ heightInView h
    , Options.css "overflow-y" "auto"
    , Options.css "display" "block"
    , Options.css "background" "LightGray"
    ]


bold : List (Options.Property c Msg)
bold =
    [ Options.css "font-weight" "1000" ]


heightInView : Int -> Options.Property c Msg
heightInView h =
    Options.css "height" (String.fromInt h ++ "vh")


sendListMsg : (a -> Msg) -> List a -> Int -> Msg
sendListMsg toMsg list index = case (ListX.getAt index list) of
                            Just v -> toMsg v
                            Nothing -> None




titleWithIcon : String -> String -> String -> Html Msg
titleWithIcon s i c =
    Options.styled Html.label
        [ Typo.headline3 ]
        [ Icon.view  [Options.css "color" c, Icon.size36 ] i, text s ]


toggle : Model -> String -> String -> String -> Bool -> Msg -> Html Msg
toggle model idx txt gp val msg =
    RadioButton.view Mdc
        idx
        model.mdc
        [ RadioButton.selected |> Options.when val
        --, RadioButton.group gp
        , Options.onClick msg
        ]
        [ text txt ]
