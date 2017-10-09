module WebMessageUpdate exposing (..)

import Json.Decode exposing (decodeString)
import List.Extra exposing (..)
import Dict as Dict
import ModelUpdate exposing (..)

import ProjectModel exposing (..)
import WebMessages exposing (..)
import DecodeWebEvent exposing (..)
import Project exposing (..)
import Views exposing (..)
import XMapTypes exposing (..)
import MapsExtraction exposing (..)

updateEvent : WebEvent -> Model -> (Model, Cmd Msg)
updateEvent evt model = case evt of
                            WEAllProjects ap -> ({ model | allProjects = ap }, Cmd.none)
                            WEError e -> (showMessage model ("Server: " ++ e), Cmd.none)
                            WEProjectContent p -> ({ model | openProjects = updateOpenProjects  p model.openProjects }, Cmd.none)
                            WEViewStatus pn v ms -> ({ model | openProjects = updateOpenViews  pn v ms model.openProjects }, Cmd.none)
                            WEViewChanged pn vn ms -> ({ model | openProjects = updateOpenViewMaps  pn vn ms model.openProjects }, Cmd.none)
                            WEMapsLoaded pn ms -> (handleMapsLoaded model ms, Cmd.none)
                            WEMapStored pn mn -> (showMessage model ("Map:" ++ (xmapNameToString mn) ++ " of project:"++ pn ++ " stored"), Cmd.none)
                            _ -> (showMessage model ("Message from server "++(toString evt)++" not recognized") , Cmd.none)


handleMapsLoaded : Model -> List XNamedMap -> Model
handleMapsLoaded model ms = case List.head ms of
                                    Just m -> { model |
                                        xmapToEdit = Just m.xmap,
                                        xmapName = Just m.xmapName,
                                        xmapType = Just (mapType m.xmap)
                                        }
                                    Nothing -> model

updateOpenProjects : Project -> List ProjectModel -> List ProjectModel
updateOpenProjects p ops = let pn = p.projectName
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
                                           ovs = pm.openViews
                                           updatedOvs = updateIf sameViewName (updateOpenViewMapsInView ms) ovs
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
