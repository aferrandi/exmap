module UIWrapper exposing (..)

import Html exposing (Html, text)
import Html.Attributes as Html
import List.Extra as ListX
import Material.Button as Button

import Material.FormField as FormField
import Material.Icon as Icon
import Material.List as List
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
         , Options.css "margin" "4px"
         ] ++ props
        )
        [ text txt ]

buttonClick : Model -> String -> String -> Msg -> Html Msg
buttonClick model index txt msg =
    buttonNoClick model index txt [ Options.onClick msg ]

buttonMaybe : Model -> String -> String -> Maybe Msg -> Html Msg
buttonMaybe model index txt mmsg =
    let
        msgProp = case mmsg of
                    Just msg -> [ Options.onClick msg ]
                    Nothing -> []
    in
        buttonNoClick model index txt msgProp


scrollableTableStyle : Int -> List (Options.Property c Msg)
scrollableTableStyle h =
    [ heightInView h
    , Options.css "overflow-y" "auto"
    , Options.css "display" "block"
    , Options.css "float" "left"
    , Options.css "background" "WhiteSmoke"
    ]


scrollableListStyle : Int -> List (List.Property m)
scrollableListStyle h =
    [ heightInView h
    , Options.css "overflow-y" "auto"
    , Options.css "display" "block"
    , Options.css "background" "WhiteSmoke"
    ]

bold : List (Options.Property c Msg)
bold = [ Options.css "font-weight" "1000" ]

heightInView : Int -> Options.Property c m
heightInView h =
    Options.css "height" (String.fromInt h ++ "vh")

sendListMsg : (a -> Msg) -> List a -> Int -> Msg
sendListMsg toMsg list index =
    case (ListX.getAt index list) of
        Just v -> toMsg v
        Nothing -> None

titleWithIcon : String -> String -> String -> Html Msg
titleWithIcon txt iconName color =
    Options.styled Html.label
        [ Typo.headline4 ]
        [ Icon.view
            [
                Options.css "margin" "4px",
                Options.css "color" color,
                Icon.size36
            ] iconName,
            text txt ]

radio : Model -> String -> String -> String -> Bool -> Msg -> Html Msg
radio model idx txt gp val msg =
     FormField.view [ Options.css "margin" "0 10px" ]
    [RadioButton.view Mdc
        idx
        model.mdc
        [ RadioButton.selected |> Options.when val
        --, RadioButton.group gp
        , Options.onClick msg
        ]
        []
        , Html.label [ Html.for idx ] [ text txt ]
        ]

