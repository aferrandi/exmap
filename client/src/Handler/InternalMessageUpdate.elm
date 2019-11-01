module Handler.InternalMessageUpdate exposing (updateInternal)

import Types.Calculation exposing (..)
import Dict as Dict
import Models.EmptyModel exposing (..)
import Models.InternalMessages exposing (..)
import List.Extra as ListX
import Transform.MapsExtraction exposing (xmapNameToString)
import Maybe as Maybe
import Handler.ModelUpdate exposing (..)
import Types.Project exposing (..)
import Models.ProjectModel exposing (..)
import Server.ServerMessaging exposing (..)
import Types.Views exposing (..)
import Models.WebMessages exposing (..)
import Transform.XMapParse exposing (..)
import Transform.XMapText exposing (..)
import Types.XMapTypes exposing (..)


updateInternal : InternalMsg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        MapToTextArea ->
            ( handleMapToTextArea model, Cmd.none )
        MapToTable ->
            ( handleMapToTable model, Cmd.none )
        OpenProject pn ->
            handleOpenProject model pn
        OpenView vn ->
            handleOpenView model vn
        TextToMapTextArea s ->
            ( updateXMapEditorModel model (\xm -> { xm | xmapEditing = Just s }), Cmd.none )
        UpdateMapName s ->
            ( handleUpdateMapName model s, Cmd.none )
        UpdateCalculationName s ->
            ( handleUpdateCalculationName model s, Cmd.none )
        UpdateViewName s ->
            ( handleUpdateViewName model s, Cmd.none )
        UpdateProjectName s ->
            ( handleUpdateProjectName model s, Cmd.none )
        UpdateViewLabel s ->
            ( handleUpdateViewLabel model s, Cmd.none )
        NewCalculationWithName cn ->
            ( handleNewCalculationWithName model cn, Cmd.none )
        NewViewWithName vn ->
            ( handleNewViewWithName model vn, Cmd.none )
        NewMapWithName mn mt ->
            ( handleNewMapWithName model mn mt, Cmd.none )
        NewProjectWithName pn ->
            handleNewProjectWithName model pn
        ShowMessage s ->
            ( showMessage model s, Cmd.none )
        ShowMapInEditor mn ->
            handleShowMapInEditor model mn
        SwitchProjectViewTo vt ->
            handleSwitchProjectViewTo model vt
        SwitchCategoryTo ct ->
            ( handleSwitchCategoryTo model ct, Cmd.none )
        TextToCalculationTextArea s ->
            ( updateCalculationEditorModel model (\cm -> { cm | calculationFormulaText = Just s }), Cmd.none )
        TextToResultNameText mn ->
            ( handleTextToResultNameText model mn, Cmd.none )
        AddMapToCalculation mn ->
            ( appendToFormulaText model (xmapNameToString mn), Cmd.none )
        AddOperationToCalculation on ->
            ( handleAddOperationToCalculation model on, Cmd.none )
        ChangeOperationMode om ->
            ( handleChangeOperationMode model om, Cmd.none )
        ChangeMapType mt ->
            ( handleChangeMapType model mt, Cmd.none )
        AddItemToView row it ->
            ( handleAddItemToView model row it, Cmd.none )
        AddRowToView ->
            ( handleAddRowToView model, Cmd.none )
        ChangeViewEditSelectedRow row ->
            ( handleChangeViewEditSelectedRow model row, Cmd.none )
        ChangeViewEditCheckedItem index ->
            ( handleChangeViewEditCheckedItem model index, Cmd.none )
        ShowDialog index ->
             ( handleShowDialog model index, Cmd.none)
        CloseDialog ->
             ( handleCloseDialog model, Cmd.none)
        CloseDialogWithError err ->
            ( showMessage model err |> closeDialog, Cmd.none )


handleChangeViewEditSelectedRow : Model -> Int -> Model
handleChangeViewEditSelectedRow model ri =
    updateViewEditorModel model (\vm -> { vm | rowToAddTo = ri })


handleUpdateCalculationName : Model -> String -> Model
handleUpdateCalculationName model s =
    updateCalculationEditorModel model (\cm -> { cm | newCalculationName = s })


handleUpdateViewName : Model -> String -> Model
handleUpdateViewName model s =
    updateViewEditorModel model (\vm -> { vm | newViewName = s })


handleUpdateMapName : Model -> String -> Model
handleUpdateMapName model s =
    updateXMapEditorModel model (\xm -> { xm | newXmapName = s })


handleUpdateProjectName : Model -> String -> Model
handleUpdateProjectName model s =
    { model | newProjectName = s }


handleUpdateViewLabel : Model -> String -> Model
handleUpdateViewLabel model s =
    updateViewEditorModel model (\vm -> { vm | labelEditing = s })


handleNewCalculationWithName : Model -> CalculationName -> Model
handleNewCalculationWithName model cn =
    closeDialog { model | calculationEditorModel = { emptyCalculationEditorModel | calculationName = Just cn } }


handleOpenProject : Model -> ProjectName -> ( Model, Cmd Msg )
handleOpenProject model pn =
    let
        command =
            case openProjectWithName model pn of
                Just pm -> Cmd.none
                Nothing -> sendToServer (WRSubscribeToProject pn)
    in
        ( { model
            | currentProject = Just pn
            , currentView = Nothing
            , xmapEditorModel = emptyXMapEditorModel
            , viewEditorModel = emptyViewEditorModel
            , calculationEditorModel = emptyCalculationEditorModel
          }
        , command
        )


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

handleNewMapWithName : Model -> XMapName -> XMapType -> Model
handleNewMapWithName model mn mt =
    closeDialog { model | xmapEditorModel = { emptyXMapEditorModel | xmapName = Just mn, xmapType = mt } }

handleNewProjectWithName : Model -> ProjectName -> ( Model, Cmd Msg )
handleNewProjectWithName model pn =
    ( closeDialog model, sendToServer (WRNewProject
                                              { projectName = model.newProjectName
                                              , calculations = []
                                              , viewNames = []
                                              , sources = []
                                              }
                                          ))

handleChangeMapType : Model -> XMapType -> Model
handleChangeMapType model mt =
    updateXMapEditorModel model (\xm -> { xm | xmapType = mt })


handleAddRowToView : Model -> Model
handleAddRowToView model =
    let
        updateView mv = case mv of
                Just v -> { v | rows = v.rows ++ [ emptyRow ] }
                Nothing -> { viewName = "", rows = [ emptyRow ] }
    in
        updateViewEditorModel model (\vm -> { vm | viewToEdit = Just (updateView vm.viewToEdit) })


handleAddItemToView : Model -> Int -> ViewItem -> Model
handleAddItemToView model ri it =
    let
        updateRow (ViewRow r) = List.append r [ it ] |> ViewRow
        updateRows rs = ListX.updateAt ri updateRow rs
        updateView mv = Maybe.map (\v -> { v | rows = updateRows v.rows }) mv
    in
        updateViewEditorModel model (\vm -> { vm | viewToEdit = updateView vm.viewToEdit })

handleChangeOperationMode : Model -> OperationMode -> Model
handleChangeOperationMode model om =
    updateCalculationEditorModel model (\cm -> { cm | operationMode = om })


handleSwitchProjectViewTo : Model -> ProjectFormType -> ( Model, Cmd Msg )
handleSwitchProjectViewTo model vt =
    let
        functionRequest : Maybe (Cmd Msg)
        functionRequest =
            case model.functions of
                Just fs -> Nothing
                Nothing -> Just (sendToServer WRFunctions)
        mapsInProjectRequest =
            Maybe.map (\pm -> sendToServer (WRMapsInProject pm.project.projectName)) (currentProjectModel model)
    in
    ( { model | currentProjectForm = vt }
    , Cmd.batch (List.filterMap identity [ functionRequest, mapsInProjectRequest ])
    )

handleSwitchCategoryTo : Model -> OperationCategory -> Model
handleSwitchCategoryTo model ct = { model | currentCategory = Just ct }


handleTextToResultNameText : Model -> String -> Model
handleTextToResultNameText model mn =
    updateCalculationEditorModel model (\cm -> { cm | resultMapName = Just mn })


handleShowMapInEditor : Model -> XMapName -> ( Model, Cmd Msg )
handleShowMapInEditor model mn =
    let
        cleanup = updateXMapEditorModel model (\mm -> { mm | xmapEditing = Nothing })
        command pn = sendToServer (WRLoadMap pn mn)
    in
        case model.currentProject of
            Just pn -> ( cleanup, command pn )
            Nothing -> ( model, Cmd.none )

handleMapToTable : Model -> Model
handleMapToTable model =
    let
        xmapEditorModel = model.xmapEditorModel
        mm = Maybe.map (textToMap xmapEditorModel.xmapType) xmapEditorModel.xmapEditing
    in
        case mm of
            Just (Ok m) -> updateXMapEditorModel model (\xm -> { xm | xmapToEdit = Just m, xmapEditing = Nothing })
            Just (Err e) -> showMessage model e
            Nothing -> model

handleAddOperationToCalculation : Model -> OperationId -> Model
handleAddOperationToCalculation model id =
    let
        text = model.functions
                |> Maybe.andThen (\fm -> Dict.get (operationIdToTuple id) fm.typesById)
                |> Maybe.map operationTypeToText
    in
        appendToFormulaText model (Maybe.withDefault " " text)

handleChangeViewEditCheckedItem : Model -> String -> Model
handleChangeViewEditCheckedItem model index =
    updateViewEditorModel model (\vm -> { vm | selectedViewCells = invertValue index vm.selectedViewCells })

invertValue : String -> Dict.Dict String (Maybe Bool) -> Dict.Dict String (Maybe Bool)
invertValue index dict =
    let
        inv existing = Maybe.map not (Maybe.withDefault (Just False) existing)
    in
        Dict.insert index (inv (Dict.get index dict)) dict


appendToFormulaText : Model -> String -> Model
appendToFormulaText model s =
    let
        updateFormulaText cm = Maybe.withDefault "" cm.calculationFormulaText ++ " " ++ s
    in
        updateCalculationEditorModel model (\cm -> { cm | calculationFormulaText = Just (updateFormulaText cm) })


operationTypeToText : OperationType -> String
operationTypeToText ot =
    let
        insidePars s = "[" ++ s ++ "]"
    in
        ot.operationId.name ++ " " ++ String.join " " (List.map (parameterTypeToText >> insidePars) ot.parametersTypes)

handleMapToTextArea : Model -> Model
handleMapToTextArea model =
    updateXMapEditorModel model (\xm -> { xm | xmapEditing = Maybe.map mapToText xm.xmapToEdit })

handleCloseDialog : Model ->  Model
handleCloseDialog = closeDialog

handleShowDialog : Model -> String -> Model
handleShowDialog model index = { model | openDialog = Just index }

emptyRow : ViewRow
emptyRow = ViewRow []

parameterTypeToText t =
    case t of
        ParameterDouble -> "double"
        ParameterInt -> "int"
        ParameterString -> "string"
        ParameterBool -> "bool"
        ParameterAny -> "any"

closeDialog : Model ->  Model
closeDialog model = { model | openDialog = Nothing }
