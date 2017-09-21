module WebMessageUpdate exposing (..)

import Json.Decode exposing (decodeString)

import ProjectModel exposing (..)
import WebMessages exposing (..)
import DecodeWebEvent exposing (..)
import Project exposing (..)
import Views exposing (..)
import XMapTypes exposing (..)
import List.Extra exposing (..)
import Dict as Dict

updateEvent : WebEvent -> Model -> (Model, Cmd Msg)
updateEvent evt model = case evt of
                            WEAllProjects ap -> ({ model | allProjects = ap }, Cmd.none)
                            WEError e -> ({ model | messages = e :: model.messages }, Cmd.none)
                            WEProjectContent p -> ({ model | openProjects = updateOpenProjects  p model.openProjects }, Cmd.none)
                            WEViewStatus pn v ms -> ({ model | openProjects = updateOpenViews  pn v ms model.openProjects }, Cmd.none)
                            otherwise -> (model , Cmd.none)

sameProjectName : ProjectName -> ProjectModel -> Bool
sameProjectName pn pm = pm.project.projectName == pn

updateOpenProjects : Project -> List ProjectModel -> List ProjectModel
updateOpenProjects p ops = let pn = p.projectName
                           in case find (sameProjectName pn) ops of
                                Just _ -> updateIf (sameProjectName pn) (\pm -> { pm | project = p }) ops
                                Nothing -> { project = p, openViews = [] } :: ops

updateOpenViews : ProjectName -> View  -> List XNamedMap -> List ProjectModel -> List ProjectModel
updateOpenViews pn v ms ops = updateIf (sameProjectName pn) (updateOpenViewsInProject v ms) ops

updateOpenViewsInProject : View  -> List XNamedMap -> ProjectModel -> ProjectModel
updateOpenViewsInProject v ms pm = let msn = Dict.fromList (List.map (\m -> (m.xmapName, m.xmap)) ms)
                                       sameViewName vm = vm.view.viewName == v.viewName
                                       ovs = pm.openViews
                                       newOvs = case find sameViewName ovs of
                                            Just _ -> updateIf sameViewName (\vm -> { vm | view = v, maps = msn }) ovs
                                            Nothing -> { view = v, maps = msn } :: ovs
                                    in {pm | openViews = newOvs }

updateWithWebEvent : String -> Model -> (Model, Cmd Msg)
updateWithWebEvent json model = let _ = Debug.log ("Event " ++ json)
                                in case decodeString webEventDecoder json of
                                      Ok evt -> updateEvent evt model
                                      Err err -> updateEvent (WEError err) model