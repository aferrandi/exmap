module WebMessageUpdate exposing (..)

import Json.Decode exposing (decodeString)
import List.Extra exposing (..)
import Dict as Dict

import ModelUpdate exposing (..)
import ProjectModel exposing (..)
import WebMessages exposing (..)
import DecodeWebEvent exposing (..)
import Project exposing (..)
import Calculation exposing (..)
import Views exposing (..)
import XMapTypes exposing (..)
import MapsExtraction exposing (..)

updateEvent : WebEvent -> Model -> (Model, Cmd Msg)
updateEvent evt model = case evt of
                            WEAllProjects ap -> ({ model | allProjects = ap }, Cmd.none)
                            WEError e -> (showMessage model ("Server Error: " ++ e), Cmd.none)
                            WEInfo i -> (showMessage model ("Server Info: " ++ i), Cmd.none)
                            WEProjectContent p -> (updateOpenProjects model (updateOpenProjectsWithProject p), Cmd.none)
                            WEProjectStored p -> (updateOpenProjects model (updateOpenProjectsWithProject p), Cmd.none)
                            WEViewStatus pn v ms -> (updateOpenProjects model (updateOpenViews  pn v ms), Cmd.none)
                            WEMapsInProject pn mns -> (handleMapsInProject model mns, Cmd.none)
                            WEViewChanged pn vn ms -> (updateOpenProjects model (updateOpenViewMaps  pn vn ms), Cmd.none)
                            WEMapsLoaded pn ms -> (handleMapsLoaded model ms, Cmd.none)
                            WEMapStored pn mn -> (showMessage model ("Map:" ++ (xmapNameToString mn) ++ " of project:"++ pn ++ " stored"), Cmd.none)
                            WEViewLoaded pn v -> (handleViewLoaded model v, Cmd.none)
                            WECalculationLoaded pn cs -> (handleCalculationLoaded model cs, Cmd.none)
                            WECalculationStored pn cn -> (showMessage model ("Calculation:" ++ cn ++ " of project:"++ pn ++ " stored"), Cmd.none)
                            WEFunctions fs -> (handleFunctions model fs, Cmd.none)
                            _ -> (showMessage model ("Message from server "++(toString evt)++" not recognized") , Cmd.none)


handleMapsLoaded : Model -> List XNamedMap -> Model
handleMapsLoaded model ms =
    case List.head ms of
        Just m -> updateXMapEditorModel model (\xm -> { xm |
            xmapToEdit = Just m.xmap,
            xmapName = Just m.xmapName,
            xmapType = mapType m.xmap
            })
        Nothing -> model

handleViewLoaded : Model -> View -> Model
handleViewLoaded model v =
    updateViewEditorModel model (\vm -> { vm |
                                            viewName = Just v.viewName
                                            , viewToEdit = Just v })

handleCalculationLoaded : Model -> CalculationSource -> Model
handleCalculationLoaded model cs =
    updateCalculationEditorModel model (\cm -> { cm |
                                                    calculationName = Just cs.calculationName,
                                                    resultMapName = Just (xmapNameToString cs.resultName),
                                                    calculationFormulaText = Just cs.formulaText
                                                    })

handleFunctions : Model -> Functions -> Model
handleFunctions model fs =  { model | functions = Just fs }

handleMapsInProject : Model -> List XMapName -> Model
handleMapsInProject model mns = { model | mapsInProject = mns }


updateOpenProjectsWithProject : Project -> List ProjectModel -> List ProjectModel
updateOpenProjectsWithProject p ops = let pn = p.projectName
                           in case find (sameProjectName pn) ops of
                                Just _ -> updateIf (sameProjectName pn) (\pm -> { pm | project = p }) ops
                                Nothing -> { project = p, openViews = [] } :: ops

updateOpenViews : ProjectName -> View  -> List XNamedMap -> List ProjectModel -> List ProjectModel
updateOpenViews pn v ms ops = updateIfProjectHasSameName pn (updateOpenViewsInProject v ms) ops

updateOpenViewsInProject : View  -> List XNamedMap -> ProjectModel -> ProjectModel
updateOpenViewsInProject v ms pm = let msn = Dict.fromList (List.map (\m -> (m.xmapName, m.xmap)) ms)
                                       sameViewName vm = vm.view.viewName == v.viewName
                                       ovs = pm.openViews
                                       newOvs = case find sameViewName ovs of
                                            Just _ -> updateIf sameViewName (\vm -> { vm | view = v, maps = msn }) ovs
                                            Nothing -> { view = v, maps = msn } :: ovs
                                    in {pm | openViews = newOvs }



updateOpenViewMaps : ProjectName -> ViewName  -> List XNamedMap -> List ProjectModel -> List ProjectModel
updateOpenViewMaps pn vn ms ops =  updateIfProjectHasSameName pn (updateOpenViewMapsInProject vn ms) ops

updateOpenViewMapsInProject : ViewName -> List XNamedMap -> ProjectModel -> ProjectModel
updateOpenViewMapsInProject vn ms pm = let sameViewName vm = vm.view.viewName == vn
                                           updatedOvs = updateIf sameViewName (updateOpenViewMapsInView ms) pm.openViews
                                        in {pm | openViews = updatedOvs }

updateOpenViewMapsInView : List XNamedMap -> ViewModel -> ViewModel
updateOpenViewMapsInView ms vm =
    let updateMaps msn ms = List.foldr (\m msni -> Dict.insert m.xmapName m.xmap msni) msn ms
    in{ vm | view = vm.view, maps = updateMaps vm.maps ms }




updateWithWebEvent : String -> Model -> (Model, Cmd Msg)
updateWithWebEvent json model = let _ = Debug.log ("Event " ++ json)
                                in case decodeString webEventDecoder json of
                                      Ok evt -> updateEvent evt model
                                      Err err -> updateEvent (WEError err) model

updateIfProjectHasSameName : ProjectName -> (ProjectModel -> ProjectModel) -> List ProjectModel -> List ProjectModel
updateIfProjectHasSameName pn m2m ops = updateIf (sameProjectName pn) m2m ops



sameProjectName : ProjectName -> ProjectModel -> Bool
sameProjectName pn pm = pm.project.projectName == pn
