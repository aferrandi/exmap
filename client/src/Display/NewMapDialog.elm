module Display.NewMapDialog exposing (..)

import Display.MdcIndexes exposing (makeIndex, mapEditorIdx)
import Display.UIWrapper exposing (heightInView)
import Html exposing (Html, text)
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Options as Options
import Material.Select as Select
import Material.TextField as TextField
import Models.InternalMessages exposing (InternalMsg(..))
import Material

import Models.ProjectModel exposing (..)
import Transform.MapsExtraction exposing (xmapNameFromString)
import Transform.TypeConversion exposing (enumToText, textToEnum)
import Types.XMapTypes exposing (XMapType(..))
import List.Extra as ListX


newMapDialog : Material.Index -> Model -> Html Msg
newMapDialog index model =
    let
        storeNewMap =
            case xmapNameFromString model.xmapEditorModel.newXmapName of
                Ok mn -> Internal (NewMapWithName mn model.xmapEditorModel.xmapType)
                Err e -> Internal (CloseDialogWithError e)
    in
        Dialog.view Mdc
            index
            model.mdc
            [ Dialog.open |> Options.when (model.openDialog == Just index)
            , Dialog.onClose (Internal CloseDialog)
            ]
            [ Options.styled Html.h2
                [ Dialog.title
                ]
                [ text "New Map"
                ]
            , Dialog.content [heightInView model.ui.heights.newMapDialog] [
                TextField.view Mdc
                (index ++ "-textfield")
                model.mdc
                [ TextField.label "Name"
                , Options.onInput (\s -> Internal (UpdateMapName s))
                ]
                []
                , xmapTypeChoice model
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
                    , Options.onClick storeNewMap
                    ]
                    [ text "OK"
                    ]
                ]
            ]

xmapTypeChoice : Model -> Html Msg
xmapTypeChoice model =
    let
        hasType t = model.xmapEditorModel.xmapType == t
        types = [TypeDouble, TypeInt, TypeString,  TypeBool, TypeDate]
        texts = ["Double", "Int", "String", "Bool", "Date"]
        changeMapTypeFromText txt = Internal (ChangeMapType (textToEnum types texts txt |> Maybe.withDefault TypeString))
        selectOptions t = Select.value (enumToText types texts t |> Maybe.withDefault "") :: (if hasType t then [Select.selected] else [])
    in
        Select.view Mdc
            (makeIndex mapEditorIdx "selXmapType")
            model.mdc
            [ Select.label "Type"
            , Select.selectedText (enumToText types texts model.xmapEditorModel.xmapType |> Maybe.withDefault "")
            , Select.onSelect changeMapTypeFromText
             ]
             (ListX.zip types texts |> List.map (\(t, txt) -> Select.option (selectOptions t) [ text txt]))
