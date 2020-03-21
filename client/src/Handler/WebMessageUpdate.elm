module Handler.WebMessageUpdate exposing (..)

import Types.Calculation exposing (..)
import Json.DecodeWebEvent exposing (..)
import Dict as Dict
import Json.Decode exposing (decodeString)
import List.Extra exposing (..)
import Transform.MapsExtraction exposing (..)
import Handler.ModelUpdate exposing (..)
import Types.Project exposing (..)
import Models.ProjectModel exposing (..)
import Types.Views exposing (..)
import Models.WebMessages exposing (..)
import Types.XMapTypes exposing (..)
import Dict.Extra as DictX


updateEvent : WebEvent -> Model -> ( Model, Cmd Msg )
updateEvent evt model =
    case evt of
        WEAllProjects ap ->
            ( { model | allProjects = ap }, Cmd.none )
        WEError e ->
            showMessage model ("Server Error: " ++ e)
        WEInfo i ->
            showMessage model ("Server Info: " ++ i)
        WEProjectContent p ->
            ( updateOpenProjects model (updateOpenProjectsWithProject p), Cmd.none )
        WEProjectStored p ->
            ( updateOpenProjects model (updateOpenProjectsWithProject p), Cmd.none )
        WEViewStatus pn v ms ->
            ( updateOpenProjects model (updateOpenViews pn v ms), Cmd.none )
        WEMapsInProject pn mns ->
            ( handleMapsInProject model mns, Cmd.none )
        WEViewChanged pn vn ms ->
            ( updateOpenProjects model (updateOpenViewMaps pn vn ms), Cmd.none )
        WEMapLoaded pn m ->
            ( handleMapLoaded model m, Cmd.none )
        WEMapAdded pn mn sz ->
            handleMapAdded model pn mn sz
        WEMapUpdated pn mn sz ->
            handleMapUpdated model pn mn sz
        WEViewLoaded pn v ->
            ( handleViewLoaded model v, Cmd.none )
        WEViewAdded pn vn ->
            handleViewAdded model pn vn
        WEViewUpdated pn vn  ->
            handleViewUpdated model pn vn
        WECalculationLoaded pn cs ->
            ( handleCalculationLoaded model cs, Cmd.none )
        WECalculationAdded pn cn ->
            handleCalculationAdded model pn cn
        WECalculationUpdated pn cn ->
            handleCalculationUpdated model pn cn
        WEFunctions fs ->
            ( handleFunctions model fs, Cmd.none )
        _ ->
            showMessage model ("Message from server " ++ Debug.toString evt ++ " not recognized")


handleMapLoaded : Model -> XNamedMap -> Model
handleMapLoaded model m =
    updateXMapEditorModel model
        (\xm ->
            { xm
                | xmapToEdit = Just m.xmap
                , xmapName = Just m.xmapName
                , xmapType = mapType m.xmap
                , isNew = False
            }
        )

cleanCalculationModel : Model ->  Model
cleanCalculationModel model = updateCalculationEditorModel model (\cm -> { cm | newCalculationName = "", isNew = False  })

handleCalculationAdded : Model -> ProjectName -> CalculationName -> (Model, Cmd Msg )
handleCalculationAdded model pn cn =
        showMessage (cleanCalculationModel model) ("Calculation:" ++ cn ++ " of project:" ++ pn ++ " added")

handleCalculationUpdated : Model -> ProjectName -> CalculationName -> (Model, Cmd Msg )
handleCalculationUpdated model pn cn =
        showMessage (cleanCalculationModel model) ("Calculation:" ++ cn ++ " of project:" ++ pn ++ " updated")

cleanMapModel : Model ->  Model
cleanMapModel model = updateXMapEditorModel model (\mm -> { mm | newXmapName = "", isNew = False })

handleMapAdded : Model -> ProjectName -> XMapName -> Int -> (Model, Cmd Msg )
handleMapAdded model pn mn sz =
    showMessage (cleanMapModel model) ("Map:" ++ xmapNameToString mn ++ " of project:" ++ pn ++ " with size " ++ String.fromInt sz ++ " added")

handleMapUpdated : Model -> ProjectName -> XMapName -> Int -> (Model, Cmd Msg )
handleMapUpdated model pn mn sz =
    showMessage (cleanMapModel model) ("Map:" ++ xmapNameToString mn ++ " of project:" ++ pn ++ " with size " ++ String.fromInt sz ++ " updated")


handleViewLoaded : Model -> View -> Model
handleViewLoaded model v =
    let
        buildViewEditItem : ViewItem -> (List ViewEditItem, ViewEditItemId) -> (List ViewEditItem, ViewEditItemId)
        buildViewEditItem vi (ro, lastId) = ({ content = vi, id = lastId + 1} :: ro, lastId + 1)
        buildViewEditRow : ViewRow -> (List ViewEditRow, ViewEditItemId) -> (List ViewEditRow, ViewEditItemId)
        buildViewEditRow r (rso, lastId) =  (List.foldr buildViewEditItem ([], lastId)  r.items) |> (\(ro, id) -> ((ViewEditRow ro r.idsType) :: rso, id))
        buildViewEditRows : List ViewRow -> ViewEditItemId -> (List ViewEditRow, ViewEditItemId)
        buildViewEditRows rsi lastId =  (List.foldr buildViewEditRow ([], lastId)  rsi) |> (\(rso, id) -> (rso, id))
        buildViewEditPair : (List ViewEditRow, ViewEditItemId) -> (ViewEdit, ViewEditItemId)
        buildViewEditPair (rso, lastId) = ({ viewName = v.viewName, rows = rso  }, lastId)
        buildViewEdit : ViewEditItemId -> (ViewEdit, ViewEditItemId)
        buildViewEdit lastId = buildViewEditRows v.rows lastId |> buildViewEditPair
    in
        updateViewEditorModel model
             (\vm ->
                buildViewEdit vm.lastViewEditItemId |> (\(vo, lastId) -> { vm
                    | viewName = Just v.viewName
                    , viewToEdit = Just vo
                    , lastViewEditItemId = lastId
                }
            ))

cleanViewModel : Model -> Model
cleanViewModel model = updateViewEditorModel model (\cm -> { cm | isNew = False  })

handleViewAdded : Model -> ProjectName -> ViewName -> (Model, Cmd Msg )
handleViewAdded model pn vn =
    showMessage (cleanViewModel model) ("View:" ++ vn ++ " of project:" ++ pn ++ " added")

handleViewUpdated : Model -> ProjectName -> ViewName -> (Model, Cmd Msg )
handleViewUpdated model pn vn =
    showMessage (cleanViewModel model) ("View:" ++ vn ++ " of project:" ++ pn ++ " updated")


handleCalculationLoaded : Model -> CalculationSource -> Model
handleCalculationLoaded model cs =
    updateCalculationEditorModel model
        (\cm ->
            { cm
                | calculationName = Just cs.calculationName
                , resultMapName = Just (xmapNameToString cs.resultName)
                , calculationFormulaText = Just cs.formulaText
            }
        )


handleFunctions : Model -> Functions -> Model
handleFunctions model fs =
    let
        operationIds = List.map (\o -> o.operationId) fs.operations
    in
        { model | functions =
                Just
                    { idsByCategory = DictX.groupBy (\id -> id.category) operationIds
                    , typesById = Dict.fromList (List.map (\o -> ( operationIdToTuple o.operationId, o )) fs.operations)
                    }
        }


handleMapsInProject : Model -> List XMapName -> Model
handleMapsInProject model mns =
    { model | mapsInProject = mns }


updateOpenProjectsWithProject : Project -> List ProjectModel -> List ProjectModel
updateOpenProjectsWithProject p ops =
    let
        pn = p.projectName
    in
        case find (sameProjectName pn) ops of
            Just _ -> updateIf (sameProjectName pn) (\pm -> { pm | project = p }) ops
            Nothing -> { project = p, openViews = [] } :: ops


updateOpenViews : ProjectName -> View -> List XNamedMap -> List ProjectModel -> List ProjectModel
updateOpenViews pn v ms ops =
    updateIfProjectHasSameName pn (updateOpenViewsInProject v ms) ops


updateOpenViewsInProject : View -> List XNamedMap -> ProjectModel -> ProjectModel
updateOpenViewsInProject v ms pm =
    let
        msn = Dict.fromList (List.map (\m -> ( m.xmapName, m.xmap )) ms)
        sameViewName vm = vm.view.viewName == v.viewName
        ovs = pm.openViews
        newOvs = case find sameViewName ovs of
                Just _ -> updateIf sameViewName (\vm -> { vm | view = v, maps = msn }) ovs
                Nothing -> { view = v, maps = msn } :: ovs
    in
        { pm | openViews = newOvs }


updateOpenViewMaps : ProjectName -> ViewName -> List XNamedMap -> List ProjectModel -> List ProjectModel
updateOpenViewMaps pn vn ms ops =
    updateIfProjectHasSameName pn (updateOpenViewMapsInProject vn ms) ops


updateOpenViewMapsInProject : ViewName -> List XNamedMap -> ProjectModel -> ProjectModel
updateOpenViewMapsInProject vn ms pm =
    let
        sameViewName vm =
            vm.view.viewName == vn

        updatedOvs =
            updateIf sameViewName (updateOpenViewMapsInView ms) pm.openViews
    in
        { pm | openViews = updatedOvs }


updateOpenViewMapsInView : List XNamedMap -> ViewModel -> ViewModel
updateOpenViewMapsInView ms vm =
    let
        updateMaps msn mss =
            List.foldr (\m msni -> Dict.insert m.xmapName m.xmap msni) msn mss
    in
        { vm | view = vm.view, maps = updateMaps vm.maps ms }


updateWithWebEvent : String -> Model -> ( Model, Cmd Msg )
updateWithWebEvent json model =
    let
        _ = Debug.log ("Event " ++ json)
    in
        case decodeString webEventDecoder json of
            Ok evt -> updateEvent evt model
            Err err -> showMessage model ("Error: " ++ Debug.toString err ++ " decoding Json " ++ json)


updateIfProjectHasSameName : ProjectName -> (ProjectModel -> ProjectModel) -> List ProjectModel -> List ProjectModel
updateIfProjectHasSameName pn m2m ops =
    updateIf (sameProjectName pn) m2m ops


sameProjectName : ProjectName -> ProjectModel -> Bool
sameProjectName pn pm =
    pm.project.projectName == pn
