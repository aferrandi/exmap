module Display.MessagesDialog exposing (messagesDialog)

import Material
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Options as Options exposing (when)
import Models.InternalMessages exposing (..)
import Display.MdcIndexes exposing (..)
import Html exposing (Html, div, text)
import Material.List as Lists
import Models.ProjectModel exposing (Model, Msg(..))
import Types.Project exposing (Error)
import Display.UIWrapper exposing (scrollableListStyle)

messagesDialog : Material.Index -> Model -> Html Msg
messagesDialog index model =
    Dialog.view  Mdc
        index
        model.mdc
        [ Dialog.open |> Options.when (model.openDialog == Just index)
        , Dialog.onClose (Internal CloseDialog)
        ]
        [ Options.styled Html.h2
            [ Dialog.title
            ]
            [ text "Messages"
            ]
        , Dialog.content
            [ Dialog.scrollable
            ]
            [
                viewMessages model
            ]
        , Dialog.actions []
            [ Button.view Mdc
                ( makeIndex projectsUIIdx "msgDlgClose")
                model.mdc
                [ Button.ripple
                , Dialog.cancel
                , Options.onClick (Internal CloseDialog)
                ]
                [ text "Close"
                ]
            ]
        ]


viewMessagesItem : Error -> Lists.ListItem Msg
viewMessagesItem msg =
    Lists.li []
        [ Lists.graphicIcon [] "inbox"
        , text msg
        ]

viewMessages : Model -> Html Msg
viewMessages model =
    Lists.ul Mdc (makeIndex projectsUIIdx "lstMsg") model.mdc  (scrollableListStyle model.ui.heights.viewMessages) (List.map viewMessagesItem model.messages)
