module Editor.View.ViewEditorTable exposing (..)

import Dict
import Display.MdcIndexes exposing (makeIndex, viewEditorIdx)
import Display.UIWrapper exposing (buttonClick, heightInView)
import Html exposing (Html, div, text)
import Material.Checkbox as Checkbox
import Material.DataTable as DataTable
import Material.LayoutGrid as LayoutGrid
import Material.Options as Options
import List.Extra as ListX
import Material.RadioButton as RadioButton
import Models.InternalMessages exposing (..)
import Transform.MapsExtraction exposing (xmapNameToString)
import Types.Views exposing (ViewEdit, ViewEditItem, ViewEditItemId, ViewEditRow(..), ViewItem(..))
import Models.ProjectModel exposing (..)

viewEditorTable : Model -> Maybe ViewEdit -> Html Msg
viewEditorTable model mv =
    case mv of
        Just v -> DataTable.table [] [ viewEditRows model v ]
        Nothing -> DataTable.table [] [ DataTable.tbody [] [] ]

viewEditRows : Model -> ViewEdit -> Html Msg
viewEditRows model v =
    let
        rows = ListX.zip (List.range 0 (List.length v.rows)) v.rows |> List.map (viewEditRowToTableCells model) |> List.map (DataTable.tr [])
    in
        div []
        [ Options.styled div [heightInView model.ui.heights.viewEditRows, Options.css "background" "WhiteSmoke"] [DataTable.tbody [] rows]
          , LayoutGrid.view [  ]
            [ LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span3Desktop, LayoutGrid.span2Phone] [ addRowButton model ]
            , LayoutGrid.cell [LayoutGrid.span1Tablet, LayoutGrid.span3Desktop, LayoutGrid.span2Phone] [ removeCellsButton model ]
            , LayoutGrid.cell [LayoutGrid.span2Tablet, LayoutGrid.span4Desktop, LayoutGrid.span4Phone] [ removeRowButton model]
            ]
        ]

viewEditRowToTableCells : Model -> ( Int, ViewEditRow ) -> List (Html Msg)
viewEditRowToTableCells model ( rowIdx, ViewEditRow row ) =
    viewChoice model rowIdx :: List.map (viewEditItem model) row

addRowButton : Model -> Html Msg
addRowButton model =
    buttonClick model (makeIndex viewEditorIdx "btnAddRow") "Add row" (Internal AddRowToView)

removeCellsButton : Model -> Html Msg
removeCellsButton model =
    let
        itemsToDelete = Dict.toList model.viewEditorModel.checkedViewEditItems
            |> List.filter (\(k, v)-> Maybe.withDefault False v)
            |> List.map (\(k, v) -> k)
    in
        buttonClick model (makeIndex viewEditorIdx "btnRemoveCells") "Remove items" (Internal (RemoveItemsFromView itemsToDelete))

removeRowButton : Model -> Html Msg
removeRowButton model =
    buttonClick model (makeIndex viewEditorIdx "btnRemoveRow") "Remove row" (Internal (RemoveRowFromView model.viewEditorModel.rowToAddTo))

viewChoice : Model -> Int -> Html Msg
viewChoice model rowI =
    DataTable.td []
        [ RadioButton.view Mdc
            (makeIndex viewEditorIdx "radChc")
            model.mdc
            [ RadioButton.selected |> Options.when (model.viewEditorModel.rowToAddTo == rowI)
            , Options.onClick (Internal (ChangeViewEditSelectedRow rowI))
            ]
            []
        ]

viewEditItem : Model -> ViewEditItem -> Html Msg
viewEditItem model item =
    case item.content of
        MapItem mn -> DataTable.td [ Options.css "background" "Coral"] [ viewEditItemCheckbox model item.id (xmapNameToString mn) ]
        LabelItem l -> DataTable.td [ Options.css "background" "DarkTurquoise"] [ viewEditItemCheckbox model item.id l ]


viewEditItemCheckbox: Model -> ViewEditItemId -> String -> Html Msg
viewEditItemCheckbox model viewItemId txt=
    let checked =
            Dict.get viewItemId model.viewEditorModel.checkedViewEditItems
                |> Maybe.withDefault Nothing
                |> Maybe.map Checkbox.checked
                |> Maybe.withDefault Options.nop
        clickHandler = Options.onClick (Internal (ChangeViewEditCheckedItem viewItemId))
        index = makeIndex viewEditorIdx ("check" ++ (String.fromInt viewItemId))
    in
        div []
        [
            Checkbox.view Mdc index model.mdc (checked :: clickHandler :: []) [],
            text txt
        ]
