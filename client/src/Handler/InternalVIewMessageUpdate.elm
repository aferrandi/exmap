module Handler.InternalVIewMessageUpdate exposing (..)

import Dict
import Handler.ModelUpdate exposing (updateViewEditorModel)
import List.Extra as ListX
import Models.EmptyModel exposing (emptyViewEditorModel)
import Models.ProjectModel exposing (Model, Msg, ViewEditorModel, ViewEditItemsChecked, closeDialog, openViewWithName)
import Models.WebMessages exposing (WebRequest(..))
import Server.ServerMessaging exposing (sendToServer)
import Set
import Types.Calculation exposing (CalculationName)
import Types.Views exposing (..)
import Dict.Extra as DictX

handleChangeViewEditSelectedRow : Model -> Int -> Model
handleChangeViewEditSelectedRow model ri =
    updateViewEditorModel model (\vm -> { vm | rowToAddTo = ri })

handleUpdateViewName : Model -> String -> Model
handleUpdateViewName model s =
    updateViewEditorModel model (\vm -> { vm | newViewName = s })

handleUpdateViewLabel : Model -> String -> Model
handleUpdateViewLabel model s =
    updateViewEditorModel model (\vm -> { vm | labelEditing = s })

handleOpenView : Model -> ViewName -> ( Model, Cmd Msg )
handleOpenView model vn =
    let
        command =
            case openViewWithName model vn of
                Just v -> Cmd.none
                Nothing -> Maybe.withDefault Cmd.none (Maybe.map (\pn -> sendToServer (WRSubscribeToView pn vn)) model.currentProject)
    in
        ( { model | currentView = Just vn }, command )

handleNewViewWithName : Model -> CalculationName -> Model
handleNewViewWithName model vn =
    closeDialog { model | viewEditorModel = { emptyViewEditorModel | viewName = Just vn, viewToEdit = Just { viewName = vn, rows = [ emptyRow ] }, isNew = True } }

handleAddRowToViewEdit : Model -> Model
handleAddRowToViewEdit model =
    let
        updateView mv = case mv of
                Just v -> { v | rows = v.rows ++ [ emptyRow ] }
                Nothing -> { viewName = "", rows = [ emptyRow ] }
    in
        updateViewEditorModel model (\vm -> { vm | viewToEdit = Just (updateView vm.viewToEdit) })

handleSelectMapIndexForViewEdit : Model -> Int  -> Model
handleSelectMapIndexForViewEdit model idx =
    updateViewEditorModel model (\vm -> { vm | selectedMapIdx = Just idx})

handleAddItemToViewEdit : Model -> Int -> ViewItem -> Model
handleAddItemToViewEdit model ri it =
    let
        buildItem id= { id = id, content = it }
        updateRow id r  = { r | items = List.append r.items [ buildItem id ] }
        updateRows rs id = ListX.updateAt ri (updateRow id) rs
        updateView mv id  = Maybe.map (\v -> { v | rows = updateRows v.rows id }) mv
    in
        updateViewEditorModel model (\vm -> { vm | viewToEdit = updateView vm.viewToEdit (vm.lastViewEditItemId + 1),lastViewEditItemId= vm.lastViewEditItemId + 1})

handleRemoveItemsFromViewEdit : Model -> List ViewEditItemId -> Model
handleRemoveItemsFromViewEdit model ids =
    let
        idsSet = Set.fromList ids
        updateRow : ViewEditRow -> ViewEditRow
        updateRow vr = { vr | items = List.filter (\it -> not (Set.member it.id idsSet)) vr.items }
        removeFromViews: ViewEdit -> ViewEdit
        removeFromViews v = { v | rows = List.map updateRow v.rows }
        removeFromChecked: ViewEditItemsChecked -> ViewEditItemsChecked
        removeFromChecked cks = DictX.removeMany idsSet cks
    in
        updateViewEditorModel model (\vm -> { vm | viewToEdit = Maybe.map removeFromViews vm.viewToEdit, checkedViewEditItems = removeFromChecked vm.checkedViewEditItems})

handleRemoveRowFromView : Model -> Int -> Model
handleRemoveRowFromView model idx =
    let
        removeRowFromView: ViewEdit -> ViewEdit
        removeRowFromView v = { v | rows = ListX.removeAt idx v.rows }
        itemsFromRow: ViewEditRow -> List ViewEditItemId
        itemsFromRow r = List.map (\i -> i.id) r.items
        itemsFromRowIndex: ViewEdit -> List ViewEditItemId
        itemsFromRowIndex v = ListX.getAt idx v.rows |> Maybe.map itemsFromRow |> Maybe.withDefault []
        removeFromChecked: Maybe ViewEdit -> ViewEditItemsChecked -> ViewEditItemsChecked
        removeFromChecked mv cks = case mv of
                Just v -> DictX.removeMany (itemsFromRowIndex v |> Set.fromList) cks
                Nothing -> cks
    in
        updateViewEditorModel model (\vm -> { vm | viewToEdit = Maybe.map removeRowFromView vm.viewToEdit, checkedViewEditItems = removeFromChecked vm.viewToEdit vm.checkedViewEditItems, rowToAddTo = 0 })


handleChangeViewEditCheckedItem : Model -> ViewEditItemId -> Model
handleChangeViewEditCheckedItem model id =
    updateViewEditorModel model (\vm -> { vm | checkedViewEditItems = invertValue id vm.checkedViewEditItems })

handleChangeIdsType : Model -> Int -> ViewRowHedersType -> Model
handleChangeIdsType model ri rt =
    let
        updateRow r  = { r | headerType = rt }
        updateRows rs = ListX.updateAt ri updateRow rs
        updateView mv = Maybe.map (\v -> { v | rows = updateRows v.rows }) mv
    in
        updateViewEditorModel model (\vm -> { vm | viewToEdit = updateView vm.viewToEdit})

invertValue : ViewEditItemId -> ViewEditItemsChecked -> ViewEditItemsChecked
invertValue index dict =
    let
        inv existing = Maybe.map not (Maybe.withDefault (Just False) existing)
    in
        Dict.insert index (inv (Dict.get index dict)) dict

emptyRow : ViewEditRow
emptyRow = ViewEditRow [] RowHasHeader
