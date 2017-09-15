module Main exposing (main)
import Html        exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import WebSocket exposing (..)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css)
import Dict as Dict exposing (..)

import XMapTypes exposing (..)
import Project exposing (..)
import Views exposing (..)
import DecodeWebEvent exposing (..)
import EncodeWebRequest exposing (..)
import WebMessages exposing (..)
import ProjectModel exposing (..)
import ProjectsUI exposing (..)


main = Html.program
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

init : (Model, Cmd Msg)
init = emptyModel ! [  sendToServer WRAllProjects ]

view : Model -> Html Msg
view model = viewProjects model
        |> Material.Scheme.top

wsUrl : String
wsUrl = "ws://localhost:3000"

sendToServer : WebRequest -> Cmd Msg
sendToServer req = WebSocket.send wsUrl (encode 0 (encodeWebRequest req))

updateEvent : WebEvent -> Model -> (Model, Cmd Msg)
updateEvent evt model = case evt of
                            WEAllProjects ap -> ({ model | allProjects = ap }, Cmd.none)
                            WEError e -> ({ model | messages = e :: model.messages }, Cmd.none)
                            WEProjectContent p -> ({ model | openProjects = updateOpenProjects  p model.openProjects }, Cmd.none)
                            otherwise -> (model , Cmd.none)

updateOpenProjects : Project -> List ProjectModel -> List ProjectModel
updateOpenProjects p ops = let (same, notSame) = List.partition (\pm -> pm.project.projectName == p.projectName) ops
                               same1 = List.head same
                               newPm = case same1 of
                                             Just fm -> { fm | project = p }
                                             Nothing -> { project = p, openViews = [] }
                           in newPm :: notSame

updateWithWebEvent : String -> Model -> (Model, Cmd Msg)
updateWithWebEvent json model = let _ = Debug.log ("Event " ++ json)
                                in case decodeString webEventDecoder json of
                                      Ok evt -> updateEvent evt model
                                      Err err -> updateEvent (WEError err) model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Receive json -> updateWithWebEvent json model
    Send req -> model ! [ sendToServer req ]
    Mdl msg_ -> Material.update Mdl msg_ model
    SelectProjectTab idx -> ({ model | projectTab = idx }, Cmd.none)
    SelectViewTab idx -> ({ model | viewTab = idx }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl Receive


