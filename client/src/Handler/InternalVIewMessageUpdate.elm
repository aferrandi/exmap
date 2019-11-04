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
    closeDialog { model | viewEditorModel = { emptyViewEditorModel | viewName = Just vn, viewToEdit = Just { viewName = vn, rows = [ emptyRow ] } } }

handleAddRowToViewEdit : Model -> Model
handleAddRowToViewEdit model =
    let
        updateView mv = case mv of
                Just v -> { v | rows = v.rows ++ [ emptyRow ] }
                Nothing -> { viewName = "", rows = [ emptyRow ] }
    in
        updateViewEditorModel model (\vm -> { vm | viewToEdit = Just (updateView vm.viewToEdit) })


handleAddItemToViewEdit : Model -> Int -> ViewItem -> Model
handleAddItemToViewEdit model ri it =
    let
        buildItem id= { id = id, content = it }
        updateRow id (ViewEditRow r)  = List.append r [ buildItem id ] |> ViewEditRow
        updateRows rs id = ListX.updateAt ri (updateRow id) rs
        updateView mv id  = Maybe.map (\v -> { v | rows = updateRows v.rows id }) mv
    in
        updateViewEditorModel model (\vm -> { vm | viewToEdit = updateView vm.viewToEdit (vm.lastViewEditItemId + 1),lastViewEditItemId= vm.lastViewEditItemId + 1})

handleRemoveItemsFromViewEdit : Model -> List ViewEditItemId -> Model
handleRemoveItemsFromViewEdit model ids =
    let
        idsSet = Set.fromList ids
        updateRow : ViewEditRow -> ViewEditRow
        updateRow (ViewEditRow vr) = List.filter (\it -> not (Set.member it.id idsSet)) vr |> ViewEditRow
        removeFromViews: ViewEdit -> ViewEdit
        removeFromViews v = { v | rows = List.map updateRow v.rows }
        removeFromChecked: ViewEditItemsChecked -> ViewEditItemsChecked
        removeFromChecked cks = DictX.removeMany idsSet cks
    in
        updateViewEditorModel model (\vm -> { vm | viewToEdit = Maybe.map removeFromViews vm.viewToEdit, checkedViewEditItems = removeFromChecked vm.checkedViewEditItems})

handleChangeViewEditCheckedItem : Model -> ViewEditItemId -> Model
handleChangeViewEditCheckedItem model id =
    updateViewEditorModel model (\vm -> { vm | checkedViewEditItems = invertValue id vm.checkedViewEditItems })

invertValue : ViewEditItemId -> ViewEditItemsChecked -> ViewEditItemsChecked
invertValue index dict =
    let
        inv existing = Maybe.map not (Maybe.withDefault (Just False) existing)
    in
        Dict.insert index (inv (Dict.get index dict)) dict

emptyRow : ViewEditRow
emptyRow = ViewEditRow []
