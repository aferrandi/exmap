module Display.NameDialog exposing (nameDialog)

import Display.UIWrapper exposing (onEnterKey)
import Html exposing (Html, text)
import Html.Events exposing (keyCode, targetValue)
import Json.Decode as Decode
import Models.InternalMessages exposing (InternalMsg(..))
import Material
import Material.Button as Button

import Material.Dialog as Dialog
import Material.Options as Options exposing (Property)
import Material.TextField as TextField
import Models.ProjectModel exposing (..)

nameDialog : Material.Index -> Model -> String -> (String -> Msg) -> Msg -> Html Msg
nameDialog index model title onInput onOk =
    Dialog.view Mdc
        index
        model.mdc
        [ Dialog.open |> Options.when (model.openDialog == Just index)
        , Dialog.onClose (Internal CloseDialog)
        ]
        [ Options.styled Html.h2
            [ Dialog.title
            ]
            [ text title
            ]
        , Dialog.content [] [
            TextField.view Mdc
            (index ++ "-textfield")
            model.mdc
            [ TextField.label "Name"
            , Options.onInput onInput
            , onEnterKey onOk
            ]
            []
            ]
            , Dialog.actions []
            [ Button.view Mdc
                (index ++ "-button-cancel")
                model.mdc
                [ Button.ripple
                , Dialog.cancel
                , Options.onClick (Internal CloseDialog)
                ]
                [ text "Cancel"
                ]
            , Button.view Mdc
                (index ++ "-button-accept")
                model.mdc
                [ Button.ripple
                , Dialog.accept
                , Options.onClick onOk
                ]
                [ text "OK"
                ]
            ]
        ]

